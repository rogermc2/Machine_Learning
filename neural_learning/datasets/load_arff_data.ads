
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with AR_Types; use AR_Types;
with IL_Types;

package Load_ARFF_Data is

   function Get_Attributes (Data : ARFF_Record) return Attribute_List;
   function Get_Description (Data : ARFF_Record) return ARFF_Header;
   function Get_Relation (Data : ARFF_Record) return String;
   procedure Load_ARFF (File_Name : String; Data : out ARFF_Record);
   function Permute (aList : IL_Types.Float_List_2D) return
      IL_Types.Float_List_2D;
   pragma Inline (Permute);

end Load_ARFF_Data;
