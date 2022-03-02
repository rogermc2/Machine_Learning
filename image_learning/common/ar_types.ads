
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with IL_Types;

package AR_Types is

   subtype ARFF_Header is IL_Types.Indef_String_List;

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

   package AR_Integer_Package is new
     Ada.Containers.Vectors (Positive, Integer);
   subtype AR_Integer_List is AR_Integer_Package.Vector;

   package AR_Real_Package is new
     Ada.Containers.Vectors (Positive, Float);
   subtype AR_Real_List is AR_Real_Package.Vector;

   use AR_Real_Package;
   package AR_Real_Package_2D is new
     Ada.Containers.Vectors (Positive, AR_Real_List);
   subtype AR_Real_List_2D is AR_Real_Package_2D.Vector;

   package AR_UB_String_Package is new
     Ada.Containers.Vectors (Positive, Unbounded_String);
   subtype AR_UB_String_List is AR_UB_String_Package.Vector;

   type AR_Data_List_Type is (Int_List_Type, Real_List_Type, String_List_Type,
                              Nominal_List_Type);

   type List_Record (List_Kind : AR_Data_List_Type) is record
      case List_Kind is
         when Int_List_Type => Integer_List : AR_Integer_List;
         when Real_List_Type => Real_List : AR_Real_List;
         when String_List_Type => String_List : AR_UB_String_List;
         when Nominal_List_Type => Nominal_List : Nominal_Data_List;
      end case;
   end record;

   package AR_Indef_Data_Package_2D is new
     Ada.Containers.Indefinite_Vectors (Positive, List_Record);
   subtype AR_Indef_List_2D is AR_Indef_Data_Package_2D.Vector;

   --  L783 declaration of return object obj
   type ARFF_Record is record
      Header  : ARFF_Header_Record;
      Data    : AR_Real_List_2D;    --  'data': []
      Target  : AR_Integer_List;
--        Columns : AR_Indef_List_2D;   --  List of columns
   end record;

end AR_Types;
