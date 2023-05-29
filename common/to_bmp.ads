
with ML_U8_Types;

package To_BMP is

   Unsupported_Image_Format : exception;

   function Process (Image_File_Name : String; Show_Name : Boolean := True)
                     return ML_U8_Types.Image_Array;

end To_BMP;
