
with Ada.Containers.Formal_Ordered_Maps;

with IL_Types;

package Utils is

   type Bunch_Data is record
      Position       : Positive := 1;
   end record;

   package Bunch_Package is new
      Ada.Containers.Formal_Ordered_Maps (Integer, Bunch_Data);

   function Gen_Batches (Num_Slices, Batch_Size : Natural;
                         Min_Batch_Size : Natural := 0)
                         return IL_Types.Integer_List;

end Utils;
