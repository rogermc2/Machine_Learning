
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with AR_Types; use AR_Types;
with ML_Types;

package Load_ARFF_Data is

   function Get_Attributes (Data : ARFF_Record) return Attribute_List;
   function Get_Description (Data : ARFF_Record) return ARFF_Header;
   function Get_Relation (Data : ARFF_Record) return String;
   procedure Load_ARFF (File_Name : String; Data : out ARFF_Record);
   function Permute (aList : ML_Types.Value_Data_Lists_2D) return
     ML_Types.Value_Data_Lists_2D;

end Load_ARFF_Data;
