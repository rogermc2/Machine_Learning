
with Ada.Containers.Formal_Ordered_Maps;

package Utils is

   type Bunch_Data is record
      Position       : Positive := 1;
   end record;

   package Bunch_Package is new
      Ada.Containers.Formal_Ordered_Maps (Integer, Bunch_Data);

end Utils;
