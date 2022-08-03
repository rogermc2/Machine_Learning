
with Ada.Containers.Vectors;

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

package Examples is

    subtype String_1 is String (1 .. 1);
    subtype String_2 is String (1 .. 2);
    subtype String_3 is String (1 .. 3);

    type String1_Array is array (1 .. 1) of String_1;
    type String21_Array is array (1 .. 2) of String_1;
    type String2_Array is array (1 .. 2) of String_2;
    type String3_Array is array (1 .. 2) of String_3;

    package String1_Package is new Ada.Containers.Vectors (Positive, String_1);
    subtype String_Array is String1_Package.Vector;

    type String_Matrix is array (1 .. 2, 1 .. 2) of String_1;
    package String2_Matrix_Package is new Ada.Containers.Vectors
      (Positive, String_Matrix);
    subtype String2_Matrix_Array is String2_Matrix_Package.Vector;

    type Binary_Record is record
        B_Binary      : Binary_List;
        B_Binary_Mat  : Binary_Matrix_List;
        B_Boolean     : Boolean_Array (1 .. 10) :=
                          (False, True, True, True, False, False, False,
                           True, True, True);
        B_Float       : Float_Array (1 .. 10) :=
                          (0.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0);
        B_Integer     : Integer_Array_List;
        B_Integer_Mat : Integer_Matrix_List;
        B_Strings     : String_Array;
        B_Strings2    : String21_Array := ("a", "b");
        B_Strings3    : String3_Array := ("abc", "def");
    end record;

    type Continuous is record
        C_Float1   : Float_Array (1 .. 1) := (1 => 10.0 ** (-5));
        C_Float2   : Float_Array (1 .. 2) := (0.0, 0.5);
        C_Float3   : Real_Float_Matrix (1 .. 2, 1 .. 1) :=
                       ((1 => 0.0), (1 => 0.5));
    end record;

    type Continuous_Multioutput is record
        CM_Float1   : Real_Float_Matrix (1 .. 1, 1 .. 2) := (1 => (0.0, 0.5));
        CM_Float2   : Real_Float_Matrix (1 .. 2, 1 .. 2) :=
                       ((0.0, 0.5), (0.5, 0.0));

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
        MCO_Strings : String2_Matrix_Array;
    end record;

    Binary_Examples                 : Binary_Record;
    Continuous_Examples             : Continuous;
    Continuous_Multioutput_Examples : Continuous_Multioutput;
    Multilabel_Indicator_Examples   : Multilabel_Indicators;
    Multiclass_Examples             : Multiclass_Record;
    Multiclass_Multioutput_Examples : Multiclass_Multioutput_Record;

    procedure Init;

end Examples;
