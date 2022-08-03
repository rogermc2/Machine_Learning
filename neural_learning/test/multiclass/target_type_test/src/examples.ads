
with Ada.Containers.Vectors;

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

package Examples is

   subtype String_1 is String (1 .. 1);
   package String1_Package is new Ada.Containers.Vectors (Positive, String_1);
   subtype String_Array is String1_Package.Vector;

   type String_Matrix is array (1 .. 2, 1 .. 2) of String_1;
   package String2_Package is new Ada.Containers.Vectors
      (Positive, String_Matrix);
   subtype String2_Array is String2_Package.Vector;

    type Binary_Record is record
        B_Binary  : Binary_List;
        B_Boolean : Boolean_Array (1 .. 10) :=
                      (False, True, True, True, False, False, False,
                       True, True, True);
        B_Float   : Float_Array (1 .. 10) :=
                      (0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0);
        B_Integer : Integer_Matrix_List;
        B_Strings : String2_Array;
    end record;

    type Multilabel_Indicators is record
        MI_Binary  : Binary_Matrix_List;
        MI_Boolean : Boolean_Matrix_List;
        MI_Float   : Real_Matrix_List;
        MI_Integer : Integer_Matrix_List;
    end record;

    type Multiclass_Record is record
        MC_Float          : Real_Vector_List;
        MC_Integer_Array  : Integer_Array_List;
        MC_Integer_Matrix : Integer_Matrix_List;
        MC_Strings        : String_Array;
    end record;

    type Multiclass_Multioutput_Record is record
        MCO_Float   : Real_Matrix_List;
        MCO_Integer : Integer_Matrix_List;
        MCO_Strings : String2_Array;
    end record;

    Binary_List                 : Binary_Record;
    Multilabel_Indicator_List   : Multilabel_Indicators;
    Multiclass_List             : Multiclass_Record;
    Multiclass_Multioutput_List : Multiclass_Multioutput_Record;

    procedure Init;

end Examples;
