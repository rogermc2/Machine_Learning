--  Based on scikit-learn/sklearn/utils/__init__.py

with Ada.Containers.Formal_Ordered_Maps;

with NL_Types; use NL_Types;

package Utils is

   type Bunch_Data is record
      Position       : Positive := 1;
   end record;

   package Bunch_Package is new
      Ada.Containers.Formal_Ordered_Maps (Integer, Bunch_Data);

   function Gen_Batches (Num_To_Slice, Batch_Size : Positive;
                         Min_Batch_Size : Natural := 0)
                         return Integer_List_2D;

end Utils;
