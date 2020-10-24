# Website

This website is built using [Docusaurus 2](https://v2.docusaurus.io/), a modern static website generator.

## Contributing

Docusaurus uses Markdown and MDX for its content, so there are almost no limits to what can be done on the site.

### Blogging

If you would like to contribute to the blog, add a new entry to the /blog directory. Please follow a metadata format described in the initial post.

### Docs

Primary documentation is located in the [/docs folder](https://github.com/facebookincubator/CG-SQL/tree/master/docs). To add a new entry, do the following:

1) Create a new `.md` file, let's say `example.md`
2) In the `example.md`, add a heading with the next metadata:
    * `id` - ID for the file to be referenced in the Docusaurus's navigation and URL structure
    * `title` - Title of the file as it will be shown on the site
    * `sidebar_label` - Label of the file for a navigation sidebar

    Example for `example.md`:

    ```
    id: example
    title: Example for CG/SQL
    sidebar_label: Example for CG/SQL
    ```
3) In [/website/sidebars.js](https://github.com/facebookincubator/CG-SQL/blob/master/website/sidebars.js), add `id` of your newly created site. You will need to either create a new subheading for the sidebar or add your new file under an existing sidebar.

    Example for `example.md`:
    `Resources: ['dev-notes', 'example'],`

### CQL Guide

Similarly to the [Docs' contributions](#docs), CQL Guide uses Markdown for its files and follow the same pattern for headers' metadata.

The only difference is that instead of using [/website/sidebars.js](https://github.com/facebookincubator/CG-SQL/blob/master/website/sidebars.js), a contributor needs to work with [/website/sidebarsGuide.js](https://github.com/facebookincubator/CG-SQL/blob/master/website/sidebars.js)
