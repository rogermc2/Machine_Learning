
--  with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ML_Types;

package Load_ARFF_Data is

   subtype ARFF_Header is ML_Types.Indef_String_List;

   type ARFF_Data_Type is (ARFF_Date, ARFF_Integer, ARFF_Numeric, ARFF_Nominal,
                           ARFF_Real, ARFF_String);
   type Nominal_Data_Type is (Nominal_Integer, Nominal_Numeric, Nominal_Real,
                              Nominal_String);

   type Nominal_Data_Record (Data_Kind : Nominal_Data_Type) is record
      Data_String          : Unbounded_String;
      case Data_Kind is
         when Nominal_Real | Nominal_Numeric =>
            Real_Data      : Float;
         when Nominal_Integer =>
            Integer_Data   : Integer;
         when Nominal_String =>
            UB_String_Data : Unbounded_String;
      end case;
   end record;

   package Nominal_Data_Package is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (Nominal_Data_Record);
   subtype Nominal_Data_List is Nominal_Data_Package.List;

   type Attribute_Record is record
      Name          : Unbounded_String;
      Data_Kind     : ARFF_Data_Type;
      Nominal_Data  : Nominal_Data_List;
      Ignore        : Boolean := False;
      Is_Row_ID     : Boolean := False;
      Is_Target     : Boolean := False;
   end record;

   package Attribute_Data_Package is new
     Ada.Containers.Vectors (Positive, Attribute_Record);
   subtype Attribute_List is Attribute_Data_Package.Vector;

   type ARFF_Header_Record is record
      Info       : ARFF_Header;       --  'description': ''
      Relation   : Unbounded_String;  --  'relation': ''
      Attributes : Attribute_List;    --  'attributes': []
   end record;

   --  L783 declaration of return object obj
   type ARFF_Record is record
      Header : ARFF_Header_Record;
      Data   : ML_Types.ARFF_Data_List_2D;    --  'data': []
   end record;

   function Get_Attributes (Data : ARFF_Record) return Attribute_List;
   function Get_Description (Data : ARFF_Record) return ARFF_Header;
   function Get_Relation (Data : ARFF_Record) return String;
   procedure Load_ARFF (File_Name : String; Data : out ARFF_Record);
   function Permute (aList : ML_Types.Value_Data_Lists_2D) return
     ML_Types.Value_Data_Lists_2D;

end Load_ARFF_Data;
