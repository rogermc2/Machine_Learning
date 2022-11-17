
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ML_Types;

package Word_Classification is

   procedure Build_Dataset (IE_Data, EI_Data : ML_Types.Unbounded_List);
   function Feature_Names return ML_Types.Unbounded_List;

end Word_Classification;
