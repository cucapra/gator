/**
 * Type definition for webgl-obj-loader
 * 
 * [Source]: https://github.com/cucapra/braid/
 */

declare module "webgl-obj-loader" {
    /**
     * A 3D object described by an .obj file.
     */
    class Mesh {
      /**
       * Vertex positions. Each chunk of 3 values in the array is one
       * position vector.
       */
      vertices: number[];
  
      /**
       * Normal vectors for each vertex. Similar to `vertices`, this is
       * a "flat" array of 3-vectors.
       */
      vertexNormals: number[];
  
      /**
       * Texture coordinate "uv" vectors. A "flat" array of 2-vectors.
       */
      textures: number[];
  
      /**
       * Cell indices for grouping the other arrays into triangles.
       */
      indices: number[];
  
      /**
       * Parse the source code for an .obj file.
       */
      constructor(objStr: Object);
    }
}