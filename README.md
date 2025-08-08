# Automerge + Grove

## Instructions

Install dependencies

```
npm install
```

Then run the dev server:

```
npm run dev
```

Now, open the app in the browser. You should see that the location hash in the
URL is updated with a document ID. If you paste this same URL into another
window (or another browser, or even another browser on another device), you should
see the same document and you should see edits being reflected in real time.

If you want to open a new document, remove the hash from the url and reload.

## Automerge Integration

The approach we take to integrating Grove with Automerge is to use Automerge as
a simple mechanism for transporting grove patches around. I.e. the Automerge
document looks like this:

```
{
  grovePatches: {
    "<patch ID>": "<JSON serialization of grove patch>"
  }
}
```

### Why JSON serialization?

The reason for using the JSON serialization rather than the actual JSON
is that it makes it easier to translate the patches which Automerge produces
into something we can hand to Grove. For composite structures like maps
and lists Automerge produces very granular patches, for example, the
patches initializing a document like this:

```
{
  foos: [{x: 1, y: 2}]
}
```

Would look a like this:

```
[
  { action: 'put', path: [ 'foos' ], value: [] },
  { action: 'insert', path: [ 'foos', 0 ], values: [ {} ] },
  { action: 'put', path: [ 'foos', 0, 'x' ], value: 1 },
  { action: 'put', path: [ 'foos', 0, 'y' ], value: 2 }
]
```

This is unnecessarily granular for grove patches and would require us to write
some irritating logic to gather the granular patches into the format Grove
expects. By using the JSON serialization we get a single patch with the entire
grove patch in it.

One slight wrinkle is that in Automerge `string` values are actually collaborative
strings under the hood, which means overhead we don't really want. To get a
non-collaborative string we use the `Automerge.ImmutableString` class for the
value of the patch.

## TODO

- Ensure "live" edges are not used where "visible" should be
- Only allow paste into holes
- Cycle handling
- Rendering other roots
- Incremental decomp
