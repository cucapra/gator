// This file lists partial type declarations for untyped modules from npm. Or,
// when it seems too hard to write reasonable type declarations, an empty
// `declare module` represents "giving up."

declare module 'canvas-orbit-camera';
declare module 'canvas-fit';
declare module 'gl-context';
declare module 'array-pack-2d';

declare module 'teapot' {
  const positions: [number, number, number][];
  const cells: [number, number, number][];
}

declare module 'normals' {
  function vertexNormals(cells: [number, number, number][], positions: [number, number, number][]): [number, number, number][];
  function faceNormals(cells: [number, number, number][], positions: [number, number, number][]): [number, number, number][];
}
