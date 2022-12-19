
with Interfaces;

package PNG_To_BMP is

   type Image_Array is array (Integer range <>, Integer range <>,
                              Integer range <>) of Interfaces.Unsigned_8;

   function Process (Image_File_Name : String) return Image_Array;

end PNG_To_BMP;
