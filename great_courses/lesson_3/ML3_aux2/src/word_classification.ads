
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ML_Types;
with NL_Types;

package Word_Classification is

   procedure Build_Dataset
     (IE_Data, EI_Data : ML_Types.Unbounded_List;
      Words, Pronounce : out ML_Types.Bounded_String_List;
      Data             : out NL_Types.Boolean_List_2D;
      Labels           : out NL_Types.Boolean_List);
   function Feature_Names return ML_Types.Unbounded_List;
   function Get_Features (Word_Line : ML_Types.String_List)
                          return NL_Types.Boolean_List;

end Word_Classification;
