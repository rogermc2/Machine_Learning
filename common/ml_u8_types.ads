
with Interfaces;

--  with ML_Arrays_And_Matrices;
--  with Python;
--  with Python_API;

package ML_U8_Types is

   type Byte_Array is array (Integer range <>) of Interfaces.Unsigned_8;

   type Unsigned_8_Array_3D is array (Integer range <>, Integer range <>,
                                      Integer range <>) of
     Interfaces.Unsigned_8;

end ML_U8_Types;
