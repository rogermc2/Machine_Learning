
with Interfaces;

package ML_U8_Types is

   type Byte_Array is array (Integer range <>) of Interfaces.Unsigned_8;

   type Unsigned_8_Array is array (Integer range <>) of Interfaces.Unsigned_8;
   type Unsigned_8_Array_2D is array (Integer range <>, Integer range <>) of
     Interfaces.Unsigned_8;
   type Unsigned_8_Array_3D is array (Integer range <>, Integer range <>,
                                      Integer range <>) of
     Interfaces.Unsigned_8;

   type Image_Array is array (Integer range <>, Integer range <>,
                              Integer range <>) of Interfaces.Unsigned_8;
   subtype Image_64_Array is ML_U8_Types.Unsigned_8_Array_3D
     (1 .. 64, 1 .. 64, 1 .. 3);
   type Image_64_Vector is array (Integer range <>) of Image_64_Array;

end ML_U8_Types;
