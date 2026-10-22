# Metadata key order

Pandoc keeps document metadata in a `Map`, so its readers lose the order of the keys and its writers sort them ([jgm/pandoc#7891](https://github.com/jgm/pandoc/issues/7891)). The library keeps the order through every conversion, so that its consumers don't have to: a document's front matter is written with its keys in the order they were read or edited in.

Pandoc still reads and writes all the metadata values. The library only records the order of the keys next to them and puts the keys back in that order on output.

## The key outline

The order is a key outline: the keys of the metadata in their order, without the values. The outline has the shape of the metadata: a map's keys each have an outline of their own, so nested keys have an order too, and a list has an outline for each of its items. Pandoc keeps the order of list items itself, so the outline of a list only gives the order of the keys inside its items, e.g. in a list of maps.

For this front matter:

<!-- prettier-ignore -->
```yaml
title: Hello
author:
- name: Ada
  affiliation: Analytical Engines
tags: [writing, editors]
```

the outline is:

- `title`
- `author`, a list with an outline for its item:
  - `name`
  - `affiliation`
- `tags`, a list with an outline for each of its two items, which have no keys

The outline always lists exactly the keys the metadata has, in their order. A key Pandoc ignores (e.g. one that ends with `_`) is left out, and a key the input gave no order for comes last, in Pandoc's sorted order. So the same metadata read from different formats has the same outline and compares equal.

## The temporary key

A Pandoc document can only carry the outline from a reader to a writer in its metadata, so the outline is stored there, serialized as JSON, under the temporary key `tmp-key-outline_`. The name ends with an underscore because Pandoc drops such keys when it reads YAML, so no front matter can have it.

This key-value pair is temporary and internal:

- It exists only between a reader and a writer, within a conversion or a diff. The diff itself compares the documents without it.
- It is never part of any output. The two writers that use it take it out of the metadata first, and every other writer gets the metadata without it.
- It is not metadata of the document. Nothing outside the library sees it, and nothing should depend on it.

## How the order travels

### Reading

A reader whose format exposes the order reads it from the input text, separately from Pandoc's parse of the values, and stores the outline under the temporary key:

- Markdown: the YAML block the document starts with.
- ProseMirror: the JSON of the document's metadata.

For other formats, the library reads no order, so their keys follow Pandoc's sorted order.

### Writing

Two writers take the outline out of the metadata and apply it:

- ProseMirror: the document's metadata is written with its keys in the outline's order.
- Markdown: Pandoc writes the document, and the entries of its front matter are rearranged to follow the outline. The library only changes where each entry is, and writes the YAML the way Pandoc does.

For the front matter above, Pandoc writes:

<!-- prettier-ignore -->
```yaml
author:
- affiliation: Analytical Engines
  name: Ada
tags:
- writing
- editors
title: Hello
```

and the library rearranges it to:

<!-- prettier-ignore -->
```yaml
title: Hello
author:
- name: Ada
  affiliation: Analytical Engines
tags:
- writing
- editors
```

### Diffing

The library diffs two documents for the history and for the steps that bring an editor up to date with another version of a document. Both take the temporary key out of the two documents before diffing them, so the diff compares only the metadata values, and a change in the order alone is no metadata change. The order still reaches the output:

- The diff holds the metadata of the "after" document, which gets its outline back. The ProseMirror document of the diff is written with its keys in that order.
- The step that updates the editor's metadata is emitted whenever the metadata of the two documents differ, outline included. A change in the order alone has the step too: without it, the editor would keep the old order and write it back with its next change.

## Limitations

- Only the order is kept. Pandoc still normalizes the rest of the front matter, e.g. it drops comments, changes the quoting and writes `[writing, editors]` as a block list.
- Only the front matter, the YAML block at the start of a Markdown document, gives an order. Pandoc also reads YAML blocks further down in a document and moves their keys into the front matter when it writes. Those keys come after the front matter's keys, sorted.
- A YAML alias, e.g. `second: *author`, has no order of its own. Pandoc writes the value of the alias out in full, and the keys of that copy come out sorted.

When Pandoc keeps the order of metadata keys itself, this can be removed without changing the output.
