
with GL.Attributes;
with GL.Objects.Buffers;
with GL.Objects.Shaders;
with GL.Objects.Textures.Targets;
with GL.Objects.Vertex_Arrays;
with GL.Uniforms;

with Program_Loader;
with Utilities;

package body Renderer is

   Quad_VAO          : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Vertex_Buffer     : GL.Objects.Buffers.Buffer;
   Model_ID          : GL.Uniforms.Uniform;
   Image_ID          : GL.Uniforms.Uniform;
   Colour_ID         : GL.Uniforms.Uniform;
   Projection_ID     : GL.Uniforms.Uniform;

   --  ------------------------------------------------------------------------

   procedure Draw_Sprite (Sprite_SP      : GL.Objects.Programs.Program;
                          aTexture       : GL.Objects.Textures.Texture;
                          Position, Size : GL.Types.Singles.Vector2;
                          Rotate         : Maths.Radian;
                          Colour         : GL.Types.Singles.Vector3) is
      use GL.Objects.Programs;
      use GL.Objects.Textures.Targets;
      use Singles;
      use Maths;
      Model_Matrix : Singles.Matrix4 := Identity4;
   begin
      --  first translate (transformations are: scale happens first then
      --  rotation then final translation happens; reversed order) ?
      Model_Matrix :=
        Scaling_Matrix ((Size (GL.X), Size (GL.Y), 1.0)) * Model_Matrix;
--        Model_Matrix := Translation_Matrix
--          ((Size (GL.X), Size (GL.Y), 0.0)) * Model_Matrix;
      Model_Matrix := Rotation_Matrix (Rotate, (0.0, 0.0, 1.0)) * Model_Matrix;
--        Model_Matrix := Translation_Matrix
--          ((Size (GL.X), Size (GL.Y), 0.0)) * Model_Matrix;
      Model_Matrix := Translation_Matrix
        ((Position (GL.X), Position (GL.Y), 0.0)) * Model_Matrix;

      Use_Program (Sprite_SP);
      GL.Uniforms.Set_Single (Model_ID, Model_Matrix);
      GL.Uniforms.Set_Single (Colour_ID, Colour);
      GL.Uniforms.Set_Int (Image_ID, 0);

      GL.Objects.Textures.Set_Active_Unit (0);
      Texture_2D.Bind (aTexture);
      Quad_VAO.Bind;

      GL.Objects.Vertex_Arrays.Draw_Arrays (Triangles, 0, 6);

   end Draw_Sprite;

   --  ------------------------------------------------------------------------

   procedure Init_Render_Data (Window : in out Glfw.Windows.Window) is
      use GL.Objects.Buffers;
      Window_Width  : Glfw.Size;
      Window_Height : Glfw.Size;
   begin
      Window.Get_Framebuffer_Size (Window_Width, Window_Height);
      Quad_VAO.Initialize_Id;
      Quad_VAO.Bind;

      Vertex_Buffer.Initialize_Id;
      Array_Buffer.Bind (Vertex_Buffer);
      Utilities.Load_Vertex_Buffer
        (Array_Buffer, Vertices, Static_Draw);

      GL.Attributes.Enable_Vertex_Attrib_Array (0);
      GL.Attributes.Set_Vertex_Attrib_Pointer (0, 4, Single_Type, True, 0, 0);
   end Init_Render_Data;

   --  ------------------------------------------------------------------------

   procedure Load_Shaders (Sprite_SP : in out GL.Objects.Programs.Program) is
      use GL.Objects.Shaders;
      use Program_Loader;
   begin
      Sprite_SP := Program_From
        ((Src ("src/shaders/sprite.vs", Vertex_Shader),
         Src ("src/shaders/sprite.fs", Fragment_Shader)));

      GL.Objects.Programs.Use_Program (Sprite_SP);
      Model_ID := GL.Objects.Programs.Uniform_Location (Sprite_SP, "model");
      Image_ID := GL.Objects.Programs.Uniform_Location (Sprite_SP, "image");
      Colour_ID := GL.Objects.Programs.Uniform_Location (Sprite_SP, "spriteColour");
      Projection_ID :=
        GL.Objects.Programs.Uniform_Location (Sprite_SP, "projection");

   end Load_Shaders;

   --  ------------------------------------------------------------------------

   procedure Set_Colour (Colour : GL.Types.Singles.Vector3) is
   begin
      GL.Uniforms.Set_Single (Colour_ID, Colour);
   end Set_Colour;

   --  ------------------------------------------------------------------------

   procedure Set_Image (Image : GL.Types.Int) is
   begin
      GL.Uniforms.Set_Int (Image_ID, Image);
   end Set_Image;

   --  ------------------------------------------------------------------------

   procedure Set_Model (Model_Matrix   : GL.Types.Singles.Matrix4) is
   begin
      GL.Uniforms.Set_Single (Model_ID, Model_Matrix);
   end Set_Model;

   --  ------------------------------------------------------------------------

   procedure Set_Perspective (Projection_Matrix  : GL.Types.Singles.Matrix4) is
   begin
      GL.Uniforms.Set_Single (Projection_ID, Projection_Matrix);
   end Set_Perspective;

   --  ------------------------------------------------------------------------

end Renderer;
