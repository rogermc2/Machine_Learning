
with Ada.Assertions; use Ada.Assertions;
with Ada.Text_IO; use Ada.Text_IO;

with Multiclass_Utils; use Multiclass_Utils;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

with Examples; use Examples;

package body Type_Tests is

    procedure Check (Name : String; Actual, Expected : Y_Type) is
    begin
        Assert (Actual = Expected,
                "Type_of_target for " & Name & " should be " &
                  Y_Type'Image (Expected) & " but got " &
                  Y_Type'Image (Actual));
    end Check;

    --  ------------------------------------------------------------------------

    procedure Binary_Tests is
        Routine_Name : constant String := "Tests.Binary_Tests ";
    begin
        Put_Line (Routine_Name);

        for index in Binary_Examples.B_Binary.First_Index ..
          Binary_Examples.B_Binary.Last_Index loop
            declare
                B_Array : constant Binary_Array :=
                            Binary_Examples.B_Binary (index);
            begin
                Check ("Binary_Array Binary ", Y_Binary,
                       Type_Of_Target (B_Array));
            end;
        end loop;

    end Binary_Tests;

    --  -------------------------------------------------------------------------

    procedure Continuous_Tests is
        Routine_Name : constant String := "Tests.Continuous_Tests ";

        procedure Vec_Test (C : Real_Float_Vector) is
        begin
            for index in C'Range loop
                Check ("Real_Float_Vector Continuous", Y_Continuous,
                       Type_Of_Target (C));
            end loop;
        end Vec_Test;

    begin
        Put_Line (Routine_Name);
        Vec_Test (Continuous_Examples.C_Float1);
        Vec_Test (Continuous_Examples.C_Float2);

        Check ("Real_Float_Matrix Continuous", Y_Continuous,
               Type_Of_Target (Continuous_Examples.C_Float3));

    end Continuous_Tests;

    --  -------------------------------------------------------------------------

    procedure Continuous_Multioutput_Tests is
        Routine_Name : constant String := "Tests.Continuous_Multioutput_Tests ";

        procedure CM_Test (CM : Real_Float_Matrix) is
        begin
            Check ("Binary Matrix Multilabel_Indicator",
                   Y_Continuous_Multioutput, Type_Of_Target (CM));
        end CM_Test;

    begin
        Put_Line (Routine_Name);
        CM_Test (Continuous_Multioutput_Examples.CM_Float1);
        CM_Test (Continuous_Multioutput_Examples.CM_Float2);

    end Continuous_Multioutput_Tests;

    --  -------------------------------------------------------------------------

    procedure Multilabel_Indicator_Tests is
        Routine_Name : constant String := "Tests.Multilabel_Indicator_Tests ";

        procedure Binary_Matrix_Test (B_Mat : Binary_Matrix) is
        begin
            Check ("Binary Matrix Multilabel_Indicator", Y_Multilabel_Indicator,
                   Type_Of_Target (B_Mat));
        end Binary_Matrix_Test;

        procedure Boolean_Matrix_Test (B_Mat : Boolean_Matrix) is
        begin
            Check ("Boolean Matrix Multilabel_Indicator",
                   Y_Multilabel_Indicator, Type_Of_Target (B_Mat));
        end Boolean_Matrix_Test;

    begin
        Put_Line (Routine_Name);
        for index in Multilabel_Indicator_Examples.MI_Binary.First_Index ..
          Multilabel_Indicator_Examples.MI_Binary.Last_Index loop
            Binary_Matrix_Test
              (Multilabel_Indicator_Examples.MI_Binary (index));
        end loop;

        for index in Multilabel_Indicator_Examples.MI_Boolean.First_Index ..
          Multilabel_Indicator_Examples.MI_Boolean.Last_Index loop
            Boolean_Matrix_Test
              (Multilabel_Indicator_Examples.MI_Boolean (index));
        end loop;

    end Multilabel_Indicator_Tests;

    --  -------------------------------------------------------------------------

end Type_Tests;
