# Tasks

## In Progress

## Pending

- [ ] Fix canvas crash: creating a canvas with multiple nodes, moving them around, then clicking causes a "recursive object" error. Likely a circular reference being passed to JSON.stringify or similar serialization in the click/move handler path (handleNodeClick, handleSlotClick, or position writing).

## Completed

- [x] Fix projector crashing and behavior bugs
