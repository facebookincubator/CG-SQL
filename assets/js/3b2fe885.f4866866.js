"use strict";(self.webpackChunkwebsite=self.webpackChunkwebsite||[]).push([[1405],{3905:function(e,n,t){t.d(n,{Zo:function(){return u},kt:function(){return h}});var a=t(7294);function i(e,n,t){return n in e?Object.defineProperty(e,n,{value:t,enumerable:!0,configurable:!0,writable:!0}):e[n]=t,e}function l(e,n){var t=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);n&&(a=a.filter((function(n){return Object.getOwnPropertyDescriptor(e,n).enumerable}))),t.push.apply(t,a)}return t}function r(e){for(var n=1;n<arguments.length;n++){var t=null!=arguments[n]?arguments[n]:{};n%2?l(Object(t),!0).forEach((function(n){i(e,n,t[n])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(t)):l(Object(t)).forEach((function(n){Object.defineProperty(e,n,Object.getOwnPropertyDescriptor(t,n))}))}return e}function o(e,n){if(null==e)return{};var t,a,i=function(e,n){if(null==e)return{};var t,a,i={},l=Object.keys(e);for(a=0;a<l.length;a++)t=l[a],n.indexOf(t)>=0||(i[t]=e[t]);return i}(e,n);if(Object.getOwnPropertySymbols){var l=Object.getOwnPropertySymbols(e);for(a=0;a<l.length;a++)t=l[a],n.indexOf(t)>=0||Object.prototype.propertyIsEnumerable.call(e,t)&&(i[t]=e[t])}return i}var p=a.createContext({}),s=function(e){var n=a.useContext(p),t=n;return e&&(t="function"==typeof e?e(n):r(r({},n),e)),t},u=function(e){var n=s(e.components);return a.createElement(p.Provider,{value:n},e.children)},c={inlineCode:"code",wrapper:function(e){var n=e.children;return a.createElement(a.Fragment,{},n)}},m=a.forwardRef((function(e,n){var t=e.components,i=e.mdxType,l=e.originalType,p=e.parentName,u=o(e,["components","mdxType","originalType","parentName"]),m=s(t),h=i,N=m["".concat(p,".").concat(h)]||m[h]||c[h]||l;return t?a.createElement(N,r(r({ref:n},u),{},{components:t})):a.createElement(N,r({ref:n},u))}));function h(e,n){var t=arguments,i=n&&n.mdxType;if("string"==typeof e||i){var l=t.length,r=new Array(l);r[0]=m;var o={};for(var p in n)hasOwnProperty.call(n,p)&&(o[p]=n[p]);o.originalType=e,o.mdxType="string"==typeof e?e:i,r[1]=o;for(var s=2;s<l;s++)r[s]=t[s];return a.createElement.apply(null,r)}return a.createElement.apply(null,t)}m.displayName="MDXCreateElement"},2116:function(e,n,t){t.r(n),t.d(n,{assets:function(){return u},contentTitle:function(){return p},default:function(){return h},frontMatter:function(){return o},metadata:function(){return s},toc:function(){return c}});var a=t(7462),i=t(3366),l=(t(7294),t(3905)),r=["components"],o={slug:"flow-analysis",title:"Control Flow Analysis in CQL",author:"CG/SQL Team",author_title:"Maintainer of CG/SQL",author_url:"https://github.com/facebookincubator",author_image_url:"https://avatars2.githubusercontent.com/u/69631?s=200&v=4",tags:["facebook","cg-sql"]},p=void 0,s={permalink:"/blog/flow-analysis",editUrl:"https://github.com/facebookincubator/CG-SQL/edit/master/website/blog/blog/2021-12-30-flow-analysis.md",source:"@site/blog/2021-12-30-flow-analysis.md",title:"Control Flow Analysis in CQL",description:"One of the biggest changes to CQL in 2021 was the addition of control flow",date:"2021-12-30T00:00:00.000Z",formattedDate:"December 30, 2021",tags:[{label:"facebook",permalink:"/blog/tags/facebook"},{label:"cg-sql",permalink:"/blog/tags/cg-sql"}],readingTime:11.395,truncated:!1,authors:[{name:"CG/SQL Team",title:"Maintainer of CG/SQL",url:"https://github.com/facebookincubator",imageURL:"https://avatars2.githubusercontent.com/u/69631?s=200&v=4"}],frontMatter:{slug:"flow-analysis",title:"Control Flow Analysis in CQL",author:"CG/SQL Team",author_title:"Maintainer of CG/SQL",author_url:"https://github.com/facebookincubator",author_image_url:"https://avatars2.githubusercontent.com/u/69631?s=200&v=4",tags:["facebook","cg-sql"]},prevItem:{title:"Using the LIKE form in the SELECT statement",permalink:"/blog/columns-sugar"},nextItem:{title:"Introducing Shared Fragments",permalink:"/blog/shared-fragments-intro"}},u={authorsImageUrls:[void 0]},c=[{value:"Improving Nullability",id:"improving-nullability",level:3},{value:"Enforcing Initialization Before Use",id:"enforcing-initialization-before-use",level:3},{value:"Understanding Control Flow Analysis in CQL",id:"understanding-control-flow-analysis-in-cql",level:3},{value:"Looking Ahead",id:"looking-ahead",level:3}],m={toc:c};function h(e){var n=e.components,t=(0,i.Z)(e,r);return(0,l.kt)("wrapper",(0,a.Z)({},m,t,{components:n,mdxType:"MDXLayout"}),(0,l.kt)("p",null,"One of the biggest changes to CQL in 2021 was the addition of control flow\nanalysis. Given an understanding of how execution can flow within a user's\nprogram, CQL can do things like infer when a nullable variable must contain a\nnonnull value and improve its type appropriately, or issue an error when a\nnonnull variable may be used before it has been initialized."),(0,l.kt)("h3",{id:"improving-nullability"},"Improving Nullability"),(0,l.kt)("p",null,"As of mid-2021, and with increasing sophistication throughout the remainder of\nthe year, CQL has been able to infer that a variable of a nullable type must not\nbe ",(0,l.kt)("inlineCode",{parentName:"p"},"NULL")," within a portion of a user's program:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-sql"},'DECLARE PROC another_proc(t0 TEXT NOT NULL, t1 TEXT NOT NULL);\n\nCREATE PROC some_proc(t0 TEXT, t1 TEXT)\nBEGIN\n  IF t0 IS NULL RETURN;\n  -- `t0` must be nonnull here if we made it this far\n\n  IF t1 IS NOT NULL THEN\n    -- `t0` and `t1` are nonnull here\n    CALL another_proc(t0, t1);\n  ELSE\n    -- `t0` is nonnull here\n    CALL another_proc(t0, "default");\n  END IF;\nEND;\n')),(0,l.kt)("p",null,"The ability of the CQL compiler to infer non-nullability greatly reduces the\nneed to use the functions ",(0,l.kt)("inlineCode",{parentName:"p"},"ifnull_crash")," and ",(0,l.kt)("inlineCode",{parentName:"p"},"ifnull_throw")," to coerce values to\na nonnull type\u2014functions that, if they are ever used incorrectly, usually result\nin programs misbehaving."),(0,l.kt)("p",null,"For a detailed description and many additional examples of what is possible\u2014CQL\ncan handle much more than what is shown above\u2014see ",(0,l.kt)("a",{parentName:"p",href:"https://cgsql.dev/cql-guide/ch03#nullability-improvements"},"the user guide's section on\nnullability\nimprovements"),"."),(0,l.kt)("h3",{id:"enforcing-initialization-before-use"},"Enforcing Initialization Before Use"),(0,l.kt)("p",null,"In CQL, it is possible to declare a variable of a nonnull type without giving it\na value. If the variable is of a non-reference type, it is assigned a default\nvalue of ",(0,l.kt)("inlineCode",{parentName:"p"},"0"),". If the variable is of a reference type (",(0,l.kt)("inlineCode",{parentName:"p"},"BLOB"),", ",(0,l.kt)("inlineCode",{parentName:"p"},"OBJECT"),", or\n",(0,l.kt)("inlineCode",{parentName:"p"},"TEXT"),"), however, it is simply set to ",(0,l.kt)("inlineCode",{parentName:"p"},"NULL")," despite the nonnull type as no\ndefault value exists."),(0,l.kt)("p",null,"To help prevent accessing a reference variable of a nonnull type and getting\nback ",(0,l.kt)("inlineCode",{parentName:"p"},"NULL"),", CQL recently began enforcing that such variables are initialized\nbefore use. The following code, therefore, now results in an error:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-sql"},"DECLARE t TEXT NOT NULL;\nCALL requires_text_notnull(t); -- error!\n")),(0,l.kt)("p",null,"Using the same engine for control flow analysis that is behind nullability\nimprovements, CQL can improve a variable to be initialized:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-sql"},'DECLARE t TEXT NOT NULL;\n\nIF some_condition THEN\n  SET t := "some example text";\n  -- `t` is initialized here\nELSE\n  THROW;\nEND IF;\n-- `t` must be initialized here if we made it this far\n\nCALL requires_text_notnull(t); -- okay!\n')),(0,l.kt)("p",null,"Thanks to CQL's ability to understand the control flow of users' programs, the\nabove example works just fine."),(0,l.kt)("p",null,"CQL now also enforces that all procedures with ",(0,l.kt)("inlineCode",{parentName:"p"},"OUT")," parameters of a nonnull\nreference type properly initialize said parameters before they return:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-sql"},"CREATE PROC some_proc(b BOOL NOT NULL, OUT t TEXT NOT NULL)\nBEGIN\n  IF b THEN\n    SET t := another_proc(t);\n    -- `t` is initialized here\n  ELSE\n    SET t := yet_another_proc(t);\n    -- `t` is initialized here\n  END IF;\n  -- `t` must be initialized here because all possible\n  -- branches initialized it, so `some_proc` is okay!\nEND;\n")),(0,l.kt)("p",null,"As with nullability improvements, understanding the nuances of what will be\nconsidered initialized is easier if one has a sense for how control flow\nanalysis works in the compiler."),(0,l.kt)("h3",{id:"understanding-control-flow-analysis-in-cql"},"Understanding Control Flow Analysis in CQL"),(0,l.kt)("p",null,"To develop an intuition for how control flow analysis works in CQL, let's begin\nby taking a look at the following example:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-sql"},'DECLARE PROC p1(OUT t TEXT NOT NULL);\nDECLARE PROC p2(i INTEGER NOT NULL, OUT t TEXT NOT NULL);\n\nCREATE PROC p0(b BOOL, i INTEGER, OUT t TEXT NOT NULL)\nBEGIN\n  IF i IS NULL THEN\n    IF b THEN\n      CALL p1(t);\n    ELSE\n      SET t := "";\n    END IF;\n    RETURN;\n  END IF;\n\n  IF i == 0 THEN\n    SET t := "";\n  ELSE IF i > 0 THEN\n    SET t := p2(i);\n  ELSE\n    THROW;\n  END IF;\nEND;\n')),(0,l.kt)("p",null,"There are a couple of things we must verify in order to ensure the code is\ntype-safe:"),(0,l.kt)("ul",null,(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("p",{parentName:"li"},"With regard to the parameters of ",(0,l.kt)("inlineCode",{parentName:"p"},"p0"),": Since ",(0,l.kt)("inlineCode",{parentName:"p"},"t")," is an ",(0,l.kt)("inlineCode",{parentName:"p"},"OUT")," parameter of type\n",(0,l.kt)("inlineCode",{parentName:"p"},"TEXT NOT NULL"),", ",(0,l.kt)("inlineCode",{parentName:"p"},"p0")," must always assign it a value before it returns. If it\ndoes not, a caller of ",(0,l.kt)("inlineCode",{parentName:"p"},"p0")," may end up with a variable of a ",(0,l.kt)("inlineCode",{parentName:"p"},"NOT NULL")," type\nthat actually contains ",(0,l.kt)("inlineCode",{parentName:"p"},"NULL"),".")),(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("p",{parentName:"li"},"With regard to the calling of ",(0,l.kt)("inlineCode",{parentName:"p"},"p2")," in ",(0,l.kt)("inlineCode",{parentName:"p"},"p0"),": Since ",(0,l.kt)("inlineCode",{parentName:"p"},"p2")," requires a first\nargument of type ",(0,l.kt)("inlineCode",{parentName:"p"},"INTEGER NOT NULL"),", some sort of check must be performed to\nensure that ",(0,l.kt)("inlineCode",{parentName:"p"},"i")," is not ",(0,l.kt)("inlineCode",{parentName:"p"},"NULL")," before ",(0,l.kt)("inlineCode",{parentName:"p"},"p2(i)")," is executed."))),(0,l.kt)("p",null,"If we carefully study ",(0,l.kt)("inlineCode",{parentName:"p"},"p0"),", we can determine that both of the above conditions\nare satisfied. Making this determination, however, is not exactly trivial, and\nreal-world code is often significantly more complicated than this\u2014and it evolves\nover time. For these reasons, having a compiler that can make such\ndeterminations automatically is critical; most modern production compilers\nperform these sorts of checks."),(0,l.kt)("p",null,"The easiest way to understand how CQL does its job is to take the above example\nline-by-line. This is not exactly how CQL works under the hood, but it should\nprovide an intuitive sense of how control flow analysis works in the compiler:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-sql"},"==> CREATE PROC p0(b BOOL, i INTEGER, OUT t TEXT NOT NULL)\n    BEGIN\n      ...\n    END;\n")),(0,l.kt)("p",null,"Right away, CQL can see that ",(0,l.kt)("inlineCode",{parentName:"p"},"t")," is declared both ",(0,l.kt)("inlineCode",{parentName:"p"},"OUT")," and ",(0,l.kt)("inlineCode",{parentName:"p"},"TEXT NOT NULL")," and\nthus requires initialization before ",(0,l.kt)("inlineCode",{parentName:"p"},"p0")," returns. CQL can, therefore, add a fact\nabout what it is analyzing to its previously null set of facts:"),(0,l.kt)("blockquote",null,(0,l.kt)("ul",{parentName:"blockquote"},(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("inlineCode",{parentName:"li"},"t")," requires initialization."))),(0,l.kt)("p",null,"We can then continue:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-sql"},"==>   IF i IS NULL THEN\n        ...\n      END IF;\n")),(0,l.kt)("p",null,"Here, the compiler notices that we're at an ",(0,l.kt)("inlineCode",{parentName:"p"},"IF")," statement. In CQL, ",(0,l.kt)("inlineCode",{parentName:"p"},"IF"),"\nstatements contain one or more ",(0,l.kt)("strong",{parentName:"p"},"branches"),", and the compiler considers every\n",(0,l.kt)("inlineCode",{parentName:"p"},"IF")," to be the start of a ",(0,l.kt)("strong",{parentName:"p"},"branch group"),". The same line also indicates the\ncondition for the first branch: ",(0,l.kt)("inlineCode",{parentName:"p"},"i IS NULL"),". CQL can update its set of facts:"),(0,l.kt)("blockquote",null,(0,l.kt)("ul",{parentName:"blockquote"},(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("inlineCode",{parentName:"li"},"t")," requires initialization."),(0,l.kt)("li",{parentName:"ul"},"In branch group:",(0,l.kt)("ul",{parentName:"li"},(0,l.kt)("li",{parentName:"ul"},"In branch when ",(0,l.kt)("inlineCode",{parentName:"li"},"i IS NULL"),":"))))),(0,l.kt)("p",null,"It then proceeds to the next line:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-sql"},'      IF i IS NULL THEN\n    ==> IF b THEN\n          CALL p1(t);\n        ELSE\n          SET t := "";\n        END IF;\n        RETURN;\n      END IF;\n')),(0,l.kt)("p",null,"Another branch group and branch:"),(0,l.kt)("blockquote",null,(0,l.kt)("ul",{parentName:"blockquote"},(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("inlineCode",{parentName:"li"},"t")," requires initialization."),(0,l.kt)("li",{parentName:"ul"},"In branch group:",(0,l.kt)("ul",{parentName:"li"},(0,l.kt)("li",{parentName:"ul"},"In branch when ",(0,l.kt)("inlineCode",{parentName:"li"},"i IS NULL"),":",(0,l.kt)("ul",{parentName:"li"},(0,l.kt)("li",{parentName:"ul"},"In branch group:",(0,l.kt)("ul",{parentName:"li"},(0,l.kt)("li",{parentName:"ul"},"In branch when ",(0,l.kt)("inlineCode",{parentName:"li"},"b"),":"))))))))),(0,l.kt)("p",null,"Continuing:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-sql"},'      IF i IS NULL THEN\n        IF b THEN\n      ==> CALL p1(t);\n        ELSE\n          SET t := "";\n        END IF;\n        RETURN;\n      END IF;\n')),(0,l.kt)("p",null,"Since ",(0,l.kt)("inlineCode",{parentName:"p"},"p1")," takes an ",(0,l.kt)("inlineCode",{parentName:"p"},"OUT")," argument of type ",(0,l.kt)("inlineCode",{parentName:"p"},"TEXT NOT NULL"),", this call\ninitializes ",(0,l.kt)("inlineCode",{parentName:"p"},"t"),", and so CQL can update its set of facts once again:"),(0,l.kt)("blockquote",null,(0,l.kt)("ul",{parentName:"blockquote"},(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("inlineCode",{parentName:"li"},"t")," requires initialization."),(0,l.kt)("li",{parentName:"ul"},"In branch group:",(0,l.kt)("ul",{parentName:"li"},(0,l.kt)("li",{parentName:"ul"},"In branch when ",(0,l.kt)("inlineCode",{parentName:"li"},"i IS NULL"),":",(0,l.kt)("ul",{parentName:"li"},(0,l.kt)("li",{parentName:"ul"},"In branch group:",(0,l.kt)("ul",{parentName:"li"},(0,l.kt)("li",{parentName:"ul"},"In branch when ",(0,l.kt)("inlineCode",{parentName:"li"},"b"),":",(0,l.kt)("ul",{parentName:"li"},(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("inlineCode",{parentName:"li"},"t")," is initialized."))))))))))),(0,l.kt)("p",null,"Jumping ahead a couple of lines:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-sql"},'      IF i IS NULL THEN\n        IF b THEN\n          CALL p1(t);\n        ELSE\n      ==> SET t := "";\n        END IF;\n        RETURN;\n      END IF;\n')),(0,l.kt)("p",null,"At this point, we're in another branch. We also have yet another fact to add\nbecause ",(0,l.kt)("inlineCode",{parentName:"p"},"t")," is initialized here as well due to the ",(0,l.kt)("inlineCode",{parentName:"p"},"SET"),":"),(0,l.kt)("blockquote",null,(0,l.kt)("ul",{parentName:"blockquote"},(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("inlineCode",{parentName:"li"},"t")," requires initialization."),(0,l.kt)("li",{parentName:"ul"},"In branch group:",(0,l.kt)("ul",{parentName:"li"},(0,l.kt)("li",{parentName:"ul"},"In branch when ",(0,l.kt)("inlineCode",{parentName:"li"},"i IS NULL"),":",(0,l.kt)("ul",{parentName:"li"},(0,l.kt)("li",{parentName:"ul"},"In branch group:",(0,l.kt)("ul",{parentName:"li"},(0,l.kt)("li",{parentName:"ul"},"In branch when ",(0,l.kt)("inlineCode",{parentName:"li"},"b"),":",(0,l.kt)("ul",{parentName:"li"},(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("inlineCode",{parentName:"li"},"t")," is initialized."))),(0,l.kt)("li",{parentName:"ul"},"In ELSE branch:",(0,l.kt)("ul",{parentName:"li"},(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("inlineCode",{parentName:"li"},"t")," is initialized."))))))))))),(0,l.kt)("p",null,"Moving ahead one more line, things get a bit more interesting:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-sql"},'      IF i IS NULL THEN\n        IF b THEN\n          CALL p1(t);\n        ELSE\n          SET t := "";\n    ==> END IF;\n        RETURN;\n      END IF;\n')),(0,l.kt)("p",null,"Here, we're at the end of an ",(0,l.kt)("inlineCode",{parentName:"p"},"IF"),", and thus the end of a branch group. Whenever\nCQL reaches the end of a branch group, it ",(0,l.kt)("em",{parentName:"p"},"merges")," the effects of all of its\nbranches."),(0,l.kt)("p",null,"One very important thing to note here is that the current branch group has an\n",(0,l.kt)("inlineCode",{parentName:"p"},"ELSE")," branch, and so the set of branches covers all possible cases. That means\nif something is initialized in every branch within the branch group, we can\nconsider it to be initialized after the branch group has ended: Initialization\nwill always occur. This allows CQL to simplify its set of facts as follows as it\nleaves the branch group:"),(0,l.kt)("blockquote",null,(0,l.kt)("ul",{parentName:"blockquote"},(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("inlineCode",{parentName:"li"},"t")," requires initialization."),(0,l.kt)("li",{parentName:"ul"},"In branch group:",(0,l.kt)("ul",{parentName:"li"},(0,l.kt)("li",{parentName:"ul"},"In branch when ",(0,l.kt)("inlineCode",{parentName:"li"},"i IS NULL"),":",(0,l.kt)("ul",{parentName:"li"},(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("inlineCode",{parentName:"li"},"t")," is initialized."))))))),(0,l.kt)("p",null,"Stepping forward one line again, we reach a ",(0,l.kt)("inlineCode",{parentName:"p"},"RETURN"),":"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-sql"},"      IF i IS NULL THEN\n        ...\n    ==> RETURN;\n      END IF;\n")),(0,l.kt)("p",null,"We're now at a point where we can exit the procedure. CQL will, therefore,\nverify that if something requires initialization, it has been initialized. Since\nwe have both the facts \"",(0,l.kt)("inlineCode",{parentName:"p"},"t"),' requires initialization" and "',(0,l.kt)("inlineCode",{parentName:"p"},"t"),' is initialized",\nall is well!'),(0,l.kt)("p",null,"The fact that the current branch returns early is added to the set of facts:"),(0,l.kt)("blockquote",null,(0,l.kt)("ul",{parentName:"blockquote"},(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("inlineCode",{parentName:"li"},"t")," requires initialization."),(0,l.kt)("li",{parentName:"ul"},"In branch group:",(0,l.kt)("ul",{parentName:"li"},(0,l.kt)("li",{parentName:"ul"},"In branch when ",(0,l.kt)("inlineCode",{parentName:"li"},"i IS NULL"),":",(0,l.kt)("ul",{parentName:"li"},(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("inlineCode",{parentName:"li"},"t")," is initialized."),(0,l.kt)("li",{parentName:"ul"},"Returns."))))))),(0,l.kt)("p",null,"Moving ahead one more line, we reach the end of another branch and branch group,\nand again something interesting happens:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-sql"},"      ...\n      IF i IS NULL THEN\n        ...\n  ==> END IF;\n")),(0,l.kt)("p",null,"Upon ending the branch group, we know that the branch group has exactly one\nbranch, that the branch is entered only when ",(0,l.kt)("inlineCode",{parentName:"p"},"i IS NULL"),", and that the branch\nreturns. What that tells CQL is that, if execution is going to continue after\nthe branch group, its sole branch must ",(0,l.kt)("em",{parentName:"p"},"not")," have been taken, and so CQL knows\nthe ",(0,l.kt)("em",{parentName:"p"},"opposite")," of its condition for entry will be true from this point onward:"),(0,l.kt)("blockquote",null,(0,l.kt)("ul",{parentName:"blockquote"},(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("inlineCode",{parentName:"li"},"t")," requires initialization."),(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("inlineCode",{parentName:"li"},"i")," is not null."))),(0,l.kt)("p",null,"The next ",(0,l.kt)("inlineCode",{parentName:"p"},"IF")," is rather similar to what we've seen already in its structure, so\nwe can jump ahead several lines to the next point of interest:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-sql"},'      IF i == 0 THEN\n        SET t := "";\n      ELSE IF i > 0 THEN\n    ==> SET t := p2(i);\n      ELSE\n        THROW;\n      END IF;\n')),(0,l.kt)("p",null,(0,l.kt)("em",{parentName:"p"},"Before")," we analyze the above-indicated line, we have the following set of facts:"),(0,l.kt)("blockquote",null,(0,l.kt)("ul",{parentName:"blockquote"},(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("inlineCode",{parentName:"li"},"t")," requires initialization."),(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("inlineCode",{parentName:"li"},"i")," is not null."),(0,l.kt)("li",{parentName:"ul"},"In branch group:",(0,l.kt)("ul",{parentName:"li"},(0,l.kt)("li",{parentName:"ul"},"In branch when ",(0,l.kt)("inlineCode",{parentName:"li"},"i == 0"),":",(0,l.kt)("ul",{parentName:"li"},(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("inlineCode",{parentName:"li"},"t")," is initialized."))),(0,l.kt)("li",{parentName:"ul"},"In branch when ",(0,l.kt)("inlineCode",{parentName:"li"},"i > 0"),":"))))),(0,l.kt)("p",null,"In the call ",(0,l.kt)("inlineCode",{parentName:"p"},"p2(i)"),", we know that ",(0,l.kt)("inlineCode",{parentName:"p"},"i")," was declared to have type ",(0,l.kt)("inlineCode",{parentName:"p"},"INTEGER")," and\nthat ",(0,l.kt)("inlineCode",{parentName:"p"},"p2")," requires an ",(0,l.kt)("inlineCode",{parentName:"p"},"INTEGER NOT NULL"),', but we also have the fact "',(0,l.kt)("inlineCode",{parentName:"p"},"i"),' is not\nnull". For this reason, we can consider ',(0,l.kt)("inlineCode",{parentName:"p"},"p2(i)")," to be a valid call. We can also\nadd the fact that ",(0,l.kt)("inlineCode",{parentName:"p"},"t")," is initialized to our current set of facts:"),(0,l.kt)("blockquote",null,(0,l.kt)("ul",{parentName:"blockquote"},(0,l.kt)("li",{parentName:"ul"},"...",(0,l.kt)("ul",{parentName:"li"},(0,l.kt)("li",{parentName:"ul"},"In branch when ",(0,l.kt)("inlineCode",{parentName:"li"},"i > 0"),":",(0,l.kt)("ul",{parentName:"li"},(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("inlineCode",{parentName:"li"},"t")," is initialized."))))))),(0,l.kt)("p",null,(0,l.kt)("strong",{parentName:"p"},"NOTE:")," When it comes to code generation, it is not so simple as to say\n",(0,l.kt)("inlineCode",{parentName:"p"},"p2(i)")," is valid and proceed as usual. That's because ",(0,l.kt)("inlineCode",{parentName:"p"},"p2")," expects an argument\nof type ",(0,l.kt)("inlineCode",{parentName:"p"},"INTEGER NOT NULL"),", but we merely have a value of type ",(0,l.kt)("inlineCode",{parentName:"p"},"INTEGER")," ",(0,l.kt)("em",{parentName:"p"},"that\nwe happen to know")," cannot be null: ",(0,l.kt)("inlineCode",{parentName:"p"},"INTEGER NOT NULL")," and ",(0,l.kt)("inlineCode",{parentName:"p"},"INTEGER")," do not share\nthe same underlying representation, and so we cannot pass the declared-nullable\nvariable ",(0,l.kt)("inlineCode",{parentName:"p"},"i")," directly to ",(0,l.kt)("inlineCode",{parentName:"p"},"p2"),". To solve this problem, CQL ",(0,l.kt)("em",{parentName:"p"},"rewrites the\nexpression")," such that ",(0,l.kt)("inlineCode",{parentName:"p"},"p2(i)")," becomes ",(0,l.kt)("inlineCode",{parentName:"p"},"p2(cql_inferred_notnull(i))"),", where\n",(0,l.kt)("inlineCode",{parentName:"p"},"cql_inferred_notnull")," is an internal-only function that handles the\nnullable-to-nonnull representational conversion for us. This explains its\npresence in the following examples."),(0,l.kt)("p",null,"Jumping ahead again, we encounter a ",(0,l.kt)("inlineCode",{parentName:"p"},"THROW"),":"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-sql"},'      IF i == 0 THEN\n        SET t := "";\n      ELSE IF i > 0 THEN\n        SET t := p2(cql_inferred_notnull(i));\n      ELSE\n    ==> THROW;\n      END IF;\n')),(0,l.kt)("p",null,"The fact that the branch will throw is added to the current set of facts:"),(0,l.kt)("blockquote",null,(0,l.kt)("ul",{parentName:"blockquote"},(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("inlineCode",{parentName:"li"},"t")," requires initialization."),(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("inlineCode",{parentName:"li"},"i")," is not null."),(0,l.kt)("li",{parentName:"ul"},"In branch group:",(0,l.kt)("ul",{parentName:"li"},(0,l.kt)("li",{parentName:"ul"},"In branch when ",(0,l.kt)("inlineCode",{parentName:"li"},"i == 0"),":",(0,l.kt)("ul",{parentName:"li"},(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("inlineCode",{parentName:"li"},"t")," is initialized."))),(0,l.kt)("li",{parentName:"ul"},"In branch when ",(0,l.kt)("inlineCode",{parentName:"li"},"i > 0"),":",(0,l.kt)("ul",{parentName:"li"},(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("inlineCode",{parentName:"li"},"t")," is initialized."))),(0,l.kt)("li",{parentName:"ul"},"In ELSE branch:",(0,l.kt)("ul",{parentName:"li"},(0,l.kt)("li",{parentName:"ul"},"Throws."))))))),(0,l.kt)("p",null,"We then proceed to the end of the ",(0,l.kt)("inlineCode",{parentName:"p"},"IF"),":"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-sql"},'      IF i == 0 THEN\n        SET t := "";\n      ELSE IF i > 0 THEN\n        SET t := p2(cql_inferred_notnull(i));\n      ELSE\n        THROW;\n  ==> END IF;\n')),(0,l.kt)("p",null,"Once again, CQL merges the effects of the branches in the branch group to finish\nthe analysis of the ",(0,l.kt)("inlineCode",{parentName:"p"},"IF"),". Since it can see that ",(0,l.kt)("inlineCode",{parentName:"p"},"t")," was initialized in all\nbranches except the one that throws, and since the branches cover all possible\ncases, the set of facts is simplified as follows given the knowledge that, if\n",(0,l.kt)("inlineCode",{parentName:"p"},"THROW")," was not encountered, ",(0,l.kt)("inlineCode",{parentName:"p"},"t")," must have been initialized:"),(0,l.kt)("blockquote",null,(0,l.kt)("ul",{parentName:"blockquote"},(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("inlineCode",{parentName:"li"},"t")," requires initialization."),(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("inlineCode",{parentName:"li"},"i")," is not null."),(0,l.kt)("li",{parentName:"ul"},(0,l.kt)("inlineCode",{parentName:"li"},"t")," is initialized."))),(0,l.kt)("p",null,"Moving ahead one final time, we encounter the end of the procedure:"),(0,l.kt)("pre",null,(0,l.kt)("code",{parentName:"pre",className:"language-sql"},"    CREATE PROC p0(b BOOL, i INTEGER, OUT t TEXT NOT NULL)\n    BEGIN\n      ...\n==> END;\n")),(0,l.kt)("p",null,'The only thing left to do at this point is to validate that anything requiring\ninitialization has been initialized. Since we have both "',(0,l.kt)("inlineCode",{parentName:"p"},"t"),' requires\ninitialization" and "',(0,l.kt)("inlineCode",{parentName:"p"},"t"),' is initialized", everything is in order.'),(0,l.kt)("h3",{id:"looking-ahead"},"Looking Ahead"),(0,l.kt)("p",null,"As a recently generalized piece of functionality within the CQL compiler,\ncontrol flow analysis will soon be used to enforce additional properties of\nusers' programs. In particular, CQL will be able to ensure that cursors are\nalways fetched before they're used and that cursors are always checked to have a\nrow before their fields are accessed."),(0,l.kt)("p",null,"Hopefully you now understand the fundamentals of control flow analysis in CQL\nand the benefits it brings to your programs. Best wishes for 2022!"))}h.isMDXComponent=!0}}]);