
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ML_Types;
with NL_Types;

package Word_Classification is

   procedure Build_Dataset
     (IE_Data, EI_Data : ML_Types.Unbounded_List;
      Labels, Words, Pronounce : out ML_Types.Unbounded_List;
      Data : out NL_Types.Boolean_List_2D);
   function Feature_Names return ML_Types.Unbounded_List;

end Word_Classification;
