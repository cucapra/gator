// This file lists partial type declarations for untyped modules from npm. Or,
// when it seems too hard to write reasonable type declarations, an empty
// `declare module` represents "giving up."

declare module 'canvas-fit' {
  /**
   * Create a resize function for a canvas that makes it fit its parent
   * element.
   *
   * @param canvas The canvas to resize.
   * @param parent Either the parent element or a function that returns the
   *               desired dimensions.
   * @param scale A factor to multiply the dimensions by (1.0 by default).
   */
  export default function fit(
    canvas: HTMLCanvasElement,
    parent?: Element | (() => [number, number]),
    scale?: number,
  ): () => void;
}

/**
 * The typed array type names from the `dtype` module on npm.
 *
 * This mapping is only used for type lookups (i.e., TypeScript's `keyof`).
 */
interface _DType {
  int8: Int8Array;
  int16: Int16Array;
  int32: Int32Array;
  uint8: Uint8Array;
  uint16: Uint16Array;
  uint32: Uint32Array;
  float32: Float32Array;
  float64: Float64Array;
  array: Array<{}>;
  uint8_clamped: Uint8ClampedArray;
}

declare module 'array-pack-2d' {
  /**
   * Flatten a 2D JavaScript array of numbers (i.e., an array of array of
   * numbers) into a typed array.
   *
   * The array is a Float32Array by default. Otherwise, you can specify a
   * different kind of array using a string.
   */
  function pack<K extends keyof _DType>(array: number[][], type: K): _DType[K];
  function pack(array: number[][]): Float32Array;  // Default.
  export default pack;
}

declare module 'canvas-orbit-camera' {
  interface Options {
    pan: boolean;
    scale: boolean;
    rotate: boolean;
  }

  /**
   * The original `orbit-camera` object.
   */
  class OrbitCamera {
    rotation: Float32Array;
    center: Float32Array;
    distance: number;

    view(out?: Float32Array): Float32Array;
    lookAt(eye: Float32Array, center: Float32Array, up: Float32Array): void;
    pan(translation: number[]): void;
    zoom(delta: number): void;
    rotate(cur: Float32Array, prev: Float32Array): void;
  }

  /**
   * A camera from `canvas-orbit-camera` augmented with a `tick` method.
   */
  class CanvasOrbitCamera extends OrbitCamera {
    /**
     * Update the camera position based on input events.
     */
    tick(): void;
  }

  export default function attachCamera(canvas: HTMLCanvasElement, opts?: Options): CanvasOrbitCamera;
}

declare module 'primitive-cube' {
  function cube(sx: number, sy: number, sz: number, ny: number, nz: number):
    {positions: [number, number, number][],
    cells: [number, number, number][]};
  export default cube;
}

declare module 'icosphere' {
  function icosphere(subdivisions: number):
    {positions: [number, number, number][],
    cells: [number, number, number][]};
  export default icosphere;
}

declare module 'stanford-dragon' {
  const positions: [number, number, number][];
  const cells: [number, number, number][];
}

declare module 'teapot' {
  const positions: [number, number, number][];
  const cells: [number, number, number][];
}

declare module 'bunny' {
  const positions: [number, number, number][];
  const cells: [number, number, number][];
}

declare module 'eye-vector' {
  function eye(view: Float32Array, out?: Float32Array): Float32Array;
  export default eye;
}

declare module 'normals' {
  function vertexNormals(cells: [number, number, number][], positions: [number, number, number][]): [number, number, number][];
  function faceNormals(cells: [number, number, number][], positions: [number, number, number][]): [number, number, number][];
}

/**
 * A *very limited* `require` function that is *only* useful for loading
 * Linguine source files via our Parcel plugin. (Normal TypeScript
 * dependencies should use `import`, not `require`.)
 */
declare function require(name: string): string;
