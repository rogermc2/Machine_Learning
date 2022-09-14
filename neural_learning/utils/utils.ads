--  Based on scikit-learn/sklearn/utils/__init__.py
--  and

with Ada.Containers.Formal_Ordered_Maps;

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types; use NL_Types;

package Utils is

   type Bunch_Data is record
      Position       : Positive := 1;
   end record;

   package Bunch_Package is new
      Ada.Containers.Formal_Ordered_Maps (Integer, Bunch_Data);

   function Sample_Without_Replacement (Population_Size, Sample_Size : Natural)
                                        return Integer_Array;
   function Gen_Batches (Num_To_Slice, Batch_Size : Positive;
                         Min_Batch_Size : Natural := 0) return Slices_List;

end Utils;
