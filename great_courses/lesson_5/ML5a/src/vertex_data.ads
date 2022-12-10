
with GL.Types;

package Vertex_Data is
   use  GL.Types;

   Quad_Vertices : constant GL.Types.Singles.Vector2_Array (1 .. 6) :=
                     ((-1.0, 1.0),   --  top left
                      (-1.0, -1.0),  --  bottom left
                      (1.0, -1.0),   --  bottom right
                      (1.0, -1.0),   --  bottom right
                      (1.0, 1.0),    --  top right
                      (-1.0, 1.0));  --  top left

   Texture_Coords : constant Singles.Vector2_Array (1 .. 6) :=
                      ((0.0, 1.0),
                       (0.0, 0.0),
                       (1.0, 0.0),
                       (1.0, 0.0),
                       (1.0, 1.0),
                       (0.0, 1.0));

end Vertex_Data;
