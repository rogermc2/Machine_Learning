
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_U8_Types;

package Support_15A is

   type String_9 is new String (1 .. 9);
   type String_9_Array is array (Integer range <>) of String_9;
   subtype Image_Array is ML_U8_Types.Unsigned_8_Array_3D
     (1 .. 64, 1 .. 64, 1 .. 3);
   type Image_Vector is array (Integer range <>) of Image_Array;
   type Labels_Array is array (Integer range <>) of Integer;

   procedure Build_Data (Num_Samples, Train_Size, Test_Size : Positive;
                         Train_X, Test_X : out Image_Vector;
                         Train_Y, Test_Y : out Integer_Array);
   function Max (Values : Real_Float_Vector) return Float;

end Support_15A;
