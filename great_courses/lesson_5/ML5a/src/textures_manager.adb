
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

with GL.Objects.Textures.Targets;
with Load_BMP_File;

package body Textures_Manager is

    procedure Load_Texture (aTexture : in out GL.Objects.Textures.Texture;
                            Image_File_Name : String; Wrap : Boolean) is
        use GL.Objects.Textures;
        use GL.Objects.Textures.Targets;
    begin
        --  Load_BMP_To_Texture initializes and binds aTexture
        Load_BMP_File.Load_BMP_To_Texture (Image_File_Name, Wrap, aTexture);
        Put_Line ("Textures_Manager.Load_Texture; image " &
                  Image_File_Name & " loaded.");
        GL.Objects.Textures.Set_Active_Unit (0);
        Texture_2D.Set_Magnifying_Filter (Nearest);
        Texture_2D.Set_Minifying_Filter (Nearest);

    exception
        when anError : others =>
            Put_Line ("An exception occurred in Textures_Manger.Load_Texture");
            Put_Line (Exception_Information (anError));
            raise;
    end Load_Texture;

end Textures_Manager;
