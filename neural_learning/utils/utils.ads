--  Based on scikit-learn/sklearn/utils/__init__.py

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
                         Min_Batch_Size           : Natural := 0) return Slices_List;

   generic
      Sample_Size : Positive;
      type Item_Type is private;
   package S_Of_N_Creator is

      subtype Index_Type is Positive range 1 .. Sample_Size;
      type Item_Array is array (Index_Type) of Item_Type;

      procedure Update (New_Item : Item_Type);
      function Result return Item_Array;

   end S_Of_N_Creator;

end Utils;
