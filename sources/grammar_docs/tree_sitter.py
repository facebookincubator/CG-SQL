#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under the MIT license found in the
# LICENSE file in the root directory of this source tree.

# ###################################################################################
# tree_sitter.py:
# ---------------
#
# Generate tree-sitter grammar for the CQL language. It's used by CQL VSCODE extension
# to provide syntax hightlighting.
#
# CQL parser (xplat/vscode/modules/tree-sitter-cql/*):
# ----------------------------------------------------
#
# It's a nuclide module that contains the parser for cql language. The parser is backed
# by the parser generator tool tree-sitter (http://tree-sitter.github.io/tree-sitter/).
# It generate a tree-sitter binary (tree-sitter-cql.wasm) from grammar.js. The binary is
# used in the CQL VSCode extension module to parse CQL files.
#
# #####################################################################################

import datetime
import re


NULL_PATTERN = re.compile(r"/\*\s*nil\s*\*/")
SEQUENCE_PATTERN = re.compile(r"\"[^\"]+\"|'.?'|[\w\-\_@]+")
WORD_PATTERN = re.compile(r"[\w\-\_@]+")
STRING_PATTERN = re.compile(r"\"[^\"]+\"")
RULE_PATTERN = re.compile(r"(.*)\s*::=\s*(.*)")
CHOICE_PATTERN = re.compile(r"\s+\|\s+")
SPACE_PATTERN = re.compile(r"\s+")
QUOTE_WORD_PATTERN = re.compile(r"'[^']+'")

PREC_LEFT = "prec.left({})"
REPEAT_1 = "repeat1({})"

# Some of the rules have conflicts therefore we need to define the precedent priority.
APPLY_FUNC_LIST = {
    "fk_target_options": PREC_LEFT,
    "math_expr": PREC_LEFT,
    "expr": PREC_LEFT,
    "join_target": PREC_LEFT,
    "elseif_list": PREC_LEFT,
    "stmt_list": REPEAT_1,
}

# these are rules in cql_grammar.txt that are not defined. We need to manually define
# them for tree sitter parser.
REPLACEMENT_RULE_NAMES = {
    "integer-literal": ["INT_LIT", "INT_LIT: $ => choice(/[0-9]+/, /0x[0-9a-fA-F]+/)"],
    "long-literal": ["LONG_LIT", "LONG_LIT: $ => choice(/[0-9]+L/, /0x[0-9a-fA-F]+L/)"],
    "real-literal": [
        "REAL_LIT",
        "REAL_LIT: $ => /([0-9]+\.[0-9]*|\.[0-9]+)((E|e)(\+|\-)?[0-9]+)?/",
    ],
    "sql-blob-literal": [
        "BLOB_LIT",
        "BLOB_LIT: $ => /[xX]'([0-9a-fA-F][0-9a-fA-F])*'/",
    ],
    "c-string-literal": [
        "C_STR_LIT",
        'C_STR_LIT: $ => /\\"(\\\\.|[^"\\n])*\\"/',
    ],
    "sql-string-literal": ["STR_LIT", "STR_LIT: $ => /'(\\\\.|''|[^'\\n])*'/"],
    "ID": ["ID", "ID: $ => /[_A-Za-z][A-Za-z0-9_]*/"],
}

# These are not part of cql_grammar.txt but are supported in cql grammar. We need to manually
# define them for tree sitter parser.
DEFAULT_RULES = [
    "comment: $ => token(choice(seq('--', /(\\\\(.|\\r?\\n)|[^\\\\\\n])*/), seq('/*', /[^*]*\*+([^/*][^*]*\*+)*/, '/')))",
    "line_directive: $ => seq(/#(line)?[ \\t]*/, $.INT_LIT, $.C_STR_LIT, /[^\\n]*/, /\\n/)",
    "macro: $ => choice($.preproc_include, $.preproc_def, $.preproc_function_def, $.preproc_call)",
    "preproc_def: $ => seq(preprocessor('define'),field('name', $.ID),field('value', optional($.preproc_arg)),'\\n')",
    "preproc_call: $ => seq(field('directive', $.preproc_directive),field('argument', optional($.preproc_arg)),'\\n')",
    "...preprocIf('', $ => $.stmt)",
    "...preprocIf('_in_field_declaration_list', $ => $.field_declaration_list_item)",
    "field_declaration_list_item: $ => choice($.declare_stmt,$.preproc_def,$.preproc_function_def,$.preproc_call,alias($.preproc_if_in_field_declaration_list, $.preproc_if),alias($.preproc_ifdef_in_field_declaration_list, $.preproc_ifdef),)",
    "preproc_include: $ => seq(preprocessor('include'),field('path', choice($.string_literal,$.system_lib_string,$.ID,alias($.preproc_call_expression, $.call_expression),)),'\\n')",
    "preproc_function_def: $ => seq(preprocessor('define'),field('name', $.ID),field('parameters', $.preproc_params),field('value', optional($.preproc_arg)),'\\n')",
    "preproc_directive: $ => /#[ \\t]*[a-zA-Z]\\w*/, preproc_arg: $ => token(prec(-1, repeat1(/.|\\\\\\r?\\n/)))",
    "preproc_expression: $ => choice($.ID,$.expr)",
    "call_expression: $ => prec(1, seq(field('function', $.expression),field('arguments', $.argument_list)))",
    "preproc_call_expression: $ => prec(1, seq(field('function', $.ID),field('arguments', alias($.preproc_argument_list, $.argument_list))))",
    "preproc_argument_list: $ => seq('(',commaSep($.preproc_expression),')')",
    "argument_list: $ => seq('(', commaSep($.expression), ')')",
    "preproc_expression: $ => $.expression",
    "expression: $ => $.expr",
    "string_literal: $ => seq(choice('L\"', 'u\"', 'U\"', 'u8\"', '\"'),repeat(choice(token.immediate(prec(1, /[^\\\\\"\\n]+/)),$.escape_sequence)),'\"',)",
    "escape_sequence: $ => token(prec(1, seq('\\\\',choice(/[^xuU]/,/\d{2,3}/,/x[0-9a-fA-F]{2,}/,/u[0-9a-fA-F]{4}/,/U[0-9a-fA-F]{8}/))))",
    "system_lib_string: $ => token(seq('<',repeat(choice(/[^>\\n]/, '\\\\>')),'>'))",
    "preproc_params: $ => seq(token.immediate('('), commaSep(choice($.ID, '...')), ')')",
]

cql_grammar = "cql_grammar.txt"
ts_grammar = {}
ts_rule_names = []

TOKEN_GRAMMAR = {}

rule_defs = {}
sorted_rule_names = []
optional_rules = set()
rules_name_visited = set()


def add_ts_rule(name, ts_rule):
    ts_grammar[name] = ts_rule
    ts_rule_names.append(name)


def get_rule_ref(token):
    if token in REPLACEMENT_RULE_NAMES:
        return "$.{}".format(REPLACEMENT_RULE_NAMES[token][0])

    if QUOTE_WORD_PATTERN.match(token):
        return token

    if STRING_PATTERN.match(token):
        tk = token.strip('"')
        if WORD_PATTERN.match(tk):
            if tk in REPLACEMENT_RULE_NAMES:
                return "$.{}".format(REPLACEMENT_RULE_NAMES[tk][0])
            name = tk.replace("@", "AT_")
            if name not in TOKEN_GRAMMAR:
                TOKEN_GRAMMAR[name] = "{}: $ => CI('{}')".format(name, tk.lower())
            return "$.{}".format(name)
        else:
            return token

    return (
        "optional($.{})".format(token)
        if token in optional_rules
        else "$.{}".format(token)
    )


def add_sub_sequence(tokens):
    name = "_".join(tokens)
    if name not in rules_name_visited:
        values = ["CI('{}')".format(item.lower()) for item in tokens]
        ts_rule = "$ => prec.left(1, seq({}))".format(", ".join(values))
        add_ts_rule(name, ts_rule)
        rules_name_visited.add(name)
    return name


# Process a subquence within a sequence. they are a group of words within a string
# e.g: "ELSE IF"
def get_sub_sequence(seq):
    tokens = SPACE_PATTERN.split(seq.strip('"'))
    name = add_sub_sequence(tokens)
    return get_rule_ref(name)


# Process a sequence in a rule.
# e.g: else_if: "else" "if"
def get_sequence(sequence):
    tokens_list = []
    for tk in sequence:
        tk = tk.strip()
        if len(tk) > 0:
            if SPACE_PATTERN.search(tk):
                tokens_list.append(get_sub_sequence(tk))
            elif STRING_PATTERN.match(tk):
                tokens_list.append(get_rule_ref(tk))
            else:
                tokens_list.append(get_rule_ref(tk))
    return tokens_list


with open(cql_grammar) as fp:
    for line in RULE_PATTERN.finditer(fp.read()):
        assert line.lastindex == 2
        name = line.group(1).strip()
        rule = line.group(2)
        choices = []
        for choice in CHOICE_PATTERN.split(rule):
            seq = []
            if NULL_PATTERN.match(choice):
                optional_rules.add(name)
            else:
                seq = [r.strip() for r in re.findall(SEQUENCE_PATTERN, choice)]
            if len(seq) > 0:
                # the rule is not optional
                choices.append(seq)
        rule_defs[name] = choices
        sorted_rule_names.append(name)

# tree_sitter.py generator is base on cql_grammar.txt which does not contains the rules for
# comment, line_directive and macro. Therefore I need to add them manually into stmt_list rule.
rule_defs["stmt_list"] = [["stmt", "';'"], ["comment"], ["line_directive"], ["macro"]]

for name in sorted_rule_names:
    if name in rules_name_visited:
        continue

    rules_name_visited.add(name)
    choices = []

    for rule in rule_defs[name]:
        seq = get_sequence(rule)
        size = len(seq)
        if size == 0:
            # An empty sequence in the rule indicates that the rule is optional.
            # We dont need to do anything here, we just move on. later it's
            # used to add the "optional()" function to optional rule's definition.
            continue
        elif size == 1:
            choices.append(seq[0])
        else:
            choices.append("seq({})".format(", ".join(seq)))

    if len(choices) == 1:
        rule_str = choices[0]
    else:
        rule_str = "choice({})".format(", ".join(choices))

    if name in APPLY_FUNC_LIST:
        rule_str = APPLY_FUNC_LIST[name].format(rule_str)
    else:
        rule_str = rule_str
    add_ts_rule(name, "$ => {}".format(rule_str))

# redefine the if_stmt rule because if not we're going to have parsing issues with "opt_elseif_list" and "opt_else" rule.
# I tried to fix it by providing a priority to the conflict but it didn't work.
ts_grammar[
    "if_stmt"
] = "$ => seq($.IF, $.expr, $.THEN, optional($.opt_stmt_list), optional(repeat1($.elseif_item)), optional($.opt_else), $.END, $.IF)"

grammar = ",\n    ".join(
    ["{}: {}".format(ts, ts_grammar[ts]) for ts in ts_rule_names]
    + DEFAULT_RULES
    + list(TOKEN_GRAMMAR.values())
    + [r[1] for r in REPLACEMENT_RULE_NAMES.values()]
)

print(
    "/**\n"
    " * Copyright (c) Meta Platforms, Inc. and affiliates.\n"
    " *\n"
    " * This source code is licensed under the MIT license found in the\n"
    " * LICENSE file in the root directory of this source tree.\n"
    " */\n\n"
)
print("// Snapshot as of {}\n\n".format(datetime.datetime.now().strftime("%c")))
print(
    "const PREC = {\n"
    "};\n\n"
    "module.exports = grammar({\n"
    "  name: 'cql',\n"
    "  extras: $ => [\n"
    "     /\\s|\\\\\\r?\\n/,\n"
    "     $.comment\n"
    "  ],\n"
    "  conflicts: $ => [\n"
    "     [$.fk_options],\n"
    "  ],\n"
    "  word: $ => $.ID,\n"
    "  rules: {"
)
print("    {}".format(grammar))
print(
    "  }\n"
    "});\n\n"
    "// make string case insensitive\n"
    "function CI (keyword) {\n"
    "  return new RegExp(keyword\n"
    "     .split('')\n"
    "     .map(letter => `[${letter}${letter.toUpperCase()}]`)\n"
    "     .join('')\n"
    "  )\n"
    "}\n"
    "// generic rule for macro directives where the directive\n"
    "// is going to be case insensitive.\n"
    "function preprocessor (command) {\n"
    "  return alias(new RegExp('#[ \\t]*' + command), '#' + command)\n"
    "}\n\n"
    "// generic rule making optional commaSep1.\n"
    "function commaSep (rule) {\n"
    "  return optional(commaSep1(rule))\n"
    "}\n\n"
    '// generic rule for a list of ID separated by ","\n'
    "function commaSep1 (rule) {\n"
    "  return seq(rule, repeat(seq(',', rule)))\n"
    "}\n\n"
    "function preprocIf (suffix, content) {\n"
    "  function elseBlock ($) {\n"
    "    return choice(\n"
    "      suffix ? alias($['preproc_else' + suffix], $.preproc_else) : $.preproc_else,\n"
    "      suffix ? alias($['preproc_elif' + suffix], $.preproc_elif) : $.preproc_elif,\n"
    "    );\n"
    "  }\n"
    "  return {\n"
    "    ['preproc_if' + suffix]: $ => seq(\n"
    "      preprocessor('if'),\n"
    "      field('condition', $.preproc_expression),\n"
    "      '\\n',\n"
    "      repeat(content($)),\n"
    "      field('alternative', optional(elseBlock($))),\n"
    "      preprocessor('endif')\n"
    "    ),\n"
    "    ['preproc_ifdef' + suffix]: $ => seq(\n"
    "      choice(preprocessor('ifdef'), preprocessor('ifndef')),\n"
    "      field('name', $.ID),\n"
    "      repeat(content($)),\n"
    "      field('alternative', optional(elseBlock($))),\n"
    "      preprocessor('endif')\n"
    "    ),\n"
    "    ['preproc_else' + suffix]: $ => seq(\n"
    "      preprocessor('else'),\n"
    "      repeat(content($))\n"
    "    ),\n"
    "    ['preproc_elif' + suffix]: $ => seq(\n"
    "      preprocessor('elif'),\n"
    "      field('condition', $.preproc_expression),\n"
    "      '\\n',\n"
    "      repeat(content($)),\n"
    "      field('alternative', optional(elseBlock($))),\n"
    "    )\n"
    "  }\n"
    "}\n"
)
