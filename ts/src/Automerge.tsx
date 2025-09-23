import {
  type Patch as AmPatch,
  isImmutableString,
  ImmutableString,
  DocHandle,
} from "@automerge/react";

/**
 * The shape of the log of patches stored in an Automerge documents
 *
 */
export type GroveDoc = {
  /**
   * A map from (unique) patch ID to the JSON serialization of the patch
   */
  grovePatches: {
    [patchId: string]: ImmutableString;
  };
};


export function groveToAutomerge(
  patches: any[],
  handle: DocHandle<GroveDoc>,
) {
  handle.change((d) => {
    for (const patch of patches) {
      // Patches are either additions or removals of an edge, so by
      // appending the sign to the edge ID we get a unique patch ID
      const patchId = `${patch.edge.id}`;
      d.grovePatches[patchId] = new ImmutableString(JSON.stringify(patch));
    }
  });
}

export function grovePatchesFromDocHandle(doc: DocHandle<GroveDoc>): any[] {
  const result = [];
  for (const [_patchId, serializedPatch] of Object.entries(
    doc.doc().grovePatches,
  )) {
    try {
      const patch = JSON.parse(serializedPatch.toString());
      result.push(patch);
    } catch (error) {
      console.error(`Error parsing patch ${_patchId}: ${error}`);
    }
  }
  return result;
}

/**
 * Convert an Automerge patch to a grove patch
 *
 * @param amPatch - A patch emitted by Automerge
 * @returns A grove patch if there was one corresponding to the Automerge patch
 */
export function amPatchToGrovePatch(amPatch: AmPatch): any | undefined {
  // The automerge document contains a map of the form:
  //
  // {
  //   "grovePatches": {
  //       "<patchId>": ImmutableString("<JSON serialized patch>")
  //    }
  // }
  //
  // Which means that automerge will emit only one kind of patch we care about,
  // a "put" patch, which will look like this:
  //
  // {
  //     action: "put",
  //     path: ["grovePatches", "<patchid>"]
  //     value: ImmutableString("<JSON serialized patch>")
  // }
  if (
    amPatch.action === "put" &&
    amPatch.path[0] === "grovePatches" &&
    amPatch.path.length === 2 &&
    typeof amPatch.path[1] === "string"
  ) {
    // const patchId = amPatch.path[1];
    if (!isImmutableString(amPatch.value)) {
      throw new Error("patches should be an ImmutableString");
    }
    const patch = JSON.parse(amPatch.value.val);
    return patch;
  }
  return;
}