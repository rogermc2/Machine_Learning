
with Interfaces;

package Simple_PNG_To_BMP is

   type BMP_Matrix is array (Integer range <>, Integer range <>) of Integer;
   type Image_Array (<>) is private;

   function As_Matrix (Image : Image_Array) return BMP_Matrix;
   function Process (Image_File_Name : String) return Image_Array;
   function Height (Data : Image_Array) return Natural;
   function Width (Data : Image_Array) return Natural;

private
   type Image_Array is array (Integer range <>, Integer range <>,
                              Integer range <>) of Interfaces.Unsigned_8;

end Simple_PNG_To_BMP;
