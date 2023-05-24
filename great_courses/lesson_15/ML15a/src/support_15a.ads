
with Interfaces;

with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;

package Support_15A is

   type String_9 is new String (1 .. 9);
   type String_9_Array is array (Integer range <>) of String_9;
   type Image_Array is array (Integer range 1 .. 64, Integer range 1 .. 64,
                              Integer range 1 .. 3) of Interfaces.Unsigned_8;
   type Image_Vector is array (Integer range <>) of Image_Array;
   type Labels_Array is array (Integer range <>) of Integer;

   procedure Read_Cats (M                     : Python.Module;
                       Cats                  : String_9_Array;
                       Label                 : Natural;
                       Train_Size, Test_Size : Float;
                        Train_X, Test_X     : out Image_Vector;
                        Train_Y, Test_Y     : out Integer_Array);
   function Max (Values : Real_Float_Vector) return Float;

end Support_15A;
