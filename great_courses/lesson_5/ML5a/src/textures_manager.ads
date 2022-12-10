
with GL.Objects.Textures;

package Textures_Manager is

   procedure Load_Texture (aTexture : in out GL.Objects.Textures.Texture;
                           Image_File_Name : String; Wrap : Boolean);

   Image_Error : Exception;

end Textures_Manager;
