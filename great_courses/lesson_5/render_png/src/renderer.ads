
with Glfw.Windows;

with GL.Objects.Programs;
with GL.Objects.Textures;
with GL.Types;

with Maths;

package Renderer is
   use  GL.Types;

   Vertices : Singles.Vector4_Array (1 .. 6) :=
   --             positions  texture coords
                ((-1.0, 1.0, 0.0, 1.0),   --  top left
                 (-1.0, -1.0, 0.0, 0.0),  --  bottom left
                 (1.0, -1.0, 1.0, 0.0),   --  bottom right
                 (1.0, -1.0, 1.0, 0.0),   --  bottom right
                 (1.0, 1.0, 1.0, 1.0),    --  top right
                 (-1.0, 1.0, 0.0, 1.0));  --  top left

   procedure Draw_Sprite (Sprite_SP      : GL.Objects.Programs.Program;
                          aTexture       : GL.Objects.Textures.Texture;
                          Position, Size : GL.Types.Singles.Vector2;
                          Rotate         : Maths.Radian;
                          Colour         : GL.Types.Singles.Vector3);

   procedure Init_Render_Data (Window : in out Glfw.Windows.Window);
   procedure Load_Shaders (Sprite_SP : in out GL.Objects.Programs.Program);
   procedure Set_Colour (Colour   : GL.Types.Singles.Vector3);
   procedure Set_Image (Image   : GL.Types.Int);
   procedure Set_Model (Model_Matrix   : GL.Types.Singles.Matrix4);
   procedure Set_Perspective (Projection_Matrix  : GL.Types.Singles.Matrix4);

end Renderer;
