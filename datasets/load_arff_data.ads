
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ML_Types;

package Load_ARFF_Data is

   type ARFF_Record is private;
   type ARFF_Data_Type is (ARFF_Numeric, ARFF_Nominal, ARFF_String, ARFF_Date);

    procedure Load_ARFF (File_Name : String; Data : out ARFF_Record);

private

   subtype ARFF_Header is ML_Types.Indef_String_List;

    type Attribute_Record is record
        Name        : Unbounded_String;
      Data_Kind     : ARFF_Data_Type;
      Nominal_Names : ML_Types.Indef_String_List;
   end record;

   package Attribute_Data_Package is new
     Ada.Containers.Doubly_Linked_Lists (Attribute_Record);
   subtype Attribute_List is Attribute_Data_Package.List;

    type ARFF_Header_Record is record
        Info       : ARFF_Header;
        Relation   : Unbounded_String;
        Attributes : Attribute_List;
   end record;

    type ARFF_Record is record
        Header : ARFF_Header_Record;
        Data   : ML_Types.Indef_String_List;
    end record;

end Load_ARFF_Data;
