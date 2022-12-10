
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Attributes;
with GL.Buffers;
with GL.Objects.Buffers;
with GL.Objects.Programs;
with GL.Objects.Textures;
with GL.Objects.Vertex_Arrays;
with GL.Objects.Shaders;
with GL.Types;
with GL.Types.Colors;
with GL.Uniforms;

with Glfw.Input;
with Glfw.Input.Keys;
with Glfw.Windows.Context;

with Program_Loader;
with Utilities;

with Textures_Manager;
with Vertex_Data;

--  ------------------------------------------------------------------------

procedure Main_Loop (Main_Window : in out Glfw.Windows.Window) is

   Background         : constant GL.Types.Colors.Color := ((0.6, 0.6, 0.6, 1.0));
   Texture_Program    : GL.Objects.Programs.Program;
   Texture_VAO        : GL.Objects.Vertex_Arrays.Vertex_Array_Object;
   Quad_Buffer        : GL.Objects.Buffers.Buffer;
   Texture_Buffer     : GL.Objects.Buffers.Buffer;
   aTexture           : GL.Objects.Textures.Texture;
   Texture_Uniform    : GL.Uniforms.Uniform;

   --  ----------------------------------------------------------------------------

   procedure Draw_Texture is
   begin
      Texture_VAO.Bind;
      GL.Objects.Vertex_Arrays.Draw_Arrays (GL.Types.Triangles, 0, 6);

   end Draw_Texture;

   --  -------------------------------------------------------------------------

   procedure Setup_Graphic is
      use GL.Attributes;
      use GL.Types;
      use Program_Loader;
      use GL.Objects.Buffers;
      use GL.Objects.Shaders;
      use Vertex_Data;
   begin
      GL.Buffers.Set_Color_Clear_Value (Background);

      Texture_Program := Program_From
        ((Src ("src/shaders/tex_vertex_shader.glsl", Vertex_Shader),
         Src ("src/shaders/tex_fragment_shader.glsl", Fragment_Shader)));
      GL.Objects.Programs.Use_Program (Texture_Program);
      Texture_Uniform :=
        GL.Objects.Programs.Uniform_Location (Texture_Program, "texture2d");
      GL.Uniforms.Set_Int (Texture_Uniform, 0);

      Texture_VAO.Initialize_Id;
      Texture_VAO.Bind;

      Quad_Buffer.Initialize_Id;
      Array_Buffer.Bind (Quad_Buffer);
      Utilities.Load_Vertex_Buffer (Array_Buffer, Quad_Vertices,
                                    Static_Draw);
      Enable_Vertex_Attrib_Array (0);
      Set_Vertex_Attrib_Pointer (0, 2, Single_Type, False, 0, 0);

      Texture_Buffer.Initialize_Id;
      Array_Buffer.Bind (Texture_Buffer);
      Utilities.Load_Vertex_Buffer (Array_Buffer, Texture_Coords,
                                    Static_Draw);
      Enable_Vertex_Attrib_Array (1);
      Set_Vertex_Attrib_Pointer (1, 2, Single_Type, False, 0, 0);

      Textures_Manager.Load_Texture (aTexture        => aTexture,
                                     Image_File_Name => "src/resources/car.bmp",
                                     Wrap            => True);

   exception
      when anError : others =>
         Put_Line ("An exception occurred in Setup_Graphic.");
         Put_Line (Exception_Information (anError));
         raise;
   end Setup_Graphic;

   --  -------------------------------------------------------------------------

   procedure Update is
   begin
      Utilities.Clear_Colour;
      Draw_Texture;
   end Update;

   --  ----------------------------------------------------------------------------

   use Glfw.Input;
   Running  : Boolean := True;
begin
   Setup_Graphic;
   while Running loop
      Update;
      Glfw.Windows.Context.Swap_Buffers (Main_Window'Access);
      Glfw.Input.Poll_Events;
      Running := Running and not
        (Main_Window.Key_State (Glfw.Input.Keys.Escape) = Glfw.Input.Pressed);
      Running := Running and not Main_Window.Should_Close;
   end loop;

exception
   when Program_Loader.Shader_Loading_Error =>
      -- message was already written to stdout
      null;
   when anError : others =>
      Put_Line ("An exception occurred in Main_Loop.");
      Put_Line (Exception_Information (anError));
      raise;
end Main_Loop;
