
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ML_Types;

package Load_ARFF_Data is

   type ARFF_Record is private;
   type ARFF_Data_Type is (ARFF_Date, ARFF_Integer, ARFF_Numeric, ARFF_Nominal,
                           ARFF_Real, ARFF_String);
   type Nominal_Data_Type is (Nominal_Integer, Nominal_Numeric, Nominal_Real,
                              Nominal_String);
   package Nominal_Types_Package is new
     Ada.Containers.Doubly_Linked_Lists (Nominal_Data_Type);
   subtype Nominal_Types_List is Nominal_Types_Package.List;

   function Get_Relation (Data : ARFF_Record) return String;
   procedure Load_ARFF (File_Name : String; Data : out ARFF_Record);

private

   subtype ARFF_Header is ML_Types.Indef_String_List;

   type Attribute_Record is record
      Name          : Unbounded_String;
      Data_Kind     : ARFF_Data_Type;
      Nominal_Names : ML_Types.Indef_String_List;
      Nominal_Types : Nominal_Types_List;
   end record;

   package Attribute_Data_Package is new
     Ada.Containers.Doubly_Linked_Lists (Attribute_Record);
   subtype Attribute_List is Attribute_Data_Package.List;

   type ARFF_Header_Record is record
      Info       : ARFF_Header;       --  'description': ''
      Relation   : Unbounded_String;  --  'relation': ''
      Attributes : Attribute_List;    --  'attributes': []
   end record;

   type ARFF_Data_Record (Data_Kind : ML_Types.Data_Type) is record
      case Data_Kind is
         when ML_Types.Boolean_Type => Boolean_Data     : Boolean;
         when ML_Types.Float_Type => Real_Data          : Float;
         when ML_Types.Integer_Type => Integer_Data     : Integer;
         when ML_Types.UB_String_Type => UB_String_Data : Unbounded_String;
      end case;
   end record;

   package ARFF_Data_Package is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (ARFF_Data_Record);
   subtype ARFF_Data_List is ARFF_Data_Package.List;

   use ARFF_Data_Package;
   package ARFF_Data_List_Package is new
     Ada.Containers.Doubly_Linked_Lists (ARFF_Data_List);
   subtype ARFF_Data_List_2D is ARFF_Data_List_Package.List;

   --  L783 declaration of return object obj
   type ARFF_Record is record
      Header : ARFF_Header_Record;
      Data   : ARFF_Data_List_2D;    --  'data': []
   end record;

end Load_ARFF_Data;
