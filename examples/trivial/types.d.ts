// This file lists partial type declarations for untyped modules from npm. Or,
// when it seems too hard to write reasonable type declarations, an empty
// `declare module` represents "giving up."

declare module 'canvas-fit';
declare module 'gl-context';
declare module 'array-pack-2d';

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

declare module 'teapot' {
  const positions: [number, number, number][];
  const cells: [number, number, number][];
}

declare module 'normals' {
  function vertexNormals(cells: [number, number, number][], positions: [number, number, number][]): [number, number, number][];
  function faceNormals(cells: [number, number, number][], positions: [number, number, number][]): [number, number, number][];
}
