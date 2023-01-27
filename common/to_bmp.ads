
with Interfaces;

package To_BMP is

   type Image_Array is array (Integer range <>, Integer range <>,
                              Integer range <>) of Interfaces.Unsigned_8;

   Unsupported_Image_Format : exception;

   function Process (Image_File_Name : String) return Image_Array;

end To_BMP;
