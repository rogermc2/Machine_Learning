
with Ada.Text_IO; use Ada.Text_IO;

with Base_Neural; use Base_Neural;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

procedure Test_Base is

    procedure Print_Float_Matrix
      (Name  : String; aMatrix : Real_Float_Matrix) is
    begin
        Put_Line (Name & ": ");
        for row in aMatrix'Range loop
            for col in aMatrix'Range (2) loop
                Put (Float'Image (aMatrix (row, col)) & "  ");
            end loop;
            New_Line;
        end loop;

    end Print_Float_Matrix;

    --  ------------------------------------------------------------------------

    Routine_Name  : constant String := "Test_Base ";
    Y_True        : constant Real_Float_Matrix (1 .. 3, 1 .. 1) :=
                      ((1 => 0.0), (1 => 0.0), (1 => 1.0));
    Y_Prob        : constant Real_Float_Matrix (1 .. 3, 1 .. 1) :=
                      ((1 => 0.9), (1 => 1.0), (1 => 1.0));
    YP_Clip       : Real_Float_Matrix := Y_Prob;

begin
    Put_Line (Routine_Name);
    Clip (YP_Clip);
    --  test_log_loss_1_prob_finite
    Put_Line ("Log loss:" & Float'Image (Log_Loss (Y_True, Y_Prob)));
    Put_Line ("Binary log loss:" &
                Float'Image (Binary_Log_Loss (Y_True, Y_Prob)));

    Print_Float_Matrix ("YP_Clip", YP_Clip);

    declare
        XLY : constant Real_Float_Matrix := X_Log_Y (Y_True, YP_Clip);
    begin
        Put_Line ("X_Log_Y width:" & Integer'Image (XLY'Length (2)));
        Print_Float_Matrix ("X_Log_Y", XLY);
    end;

end Test_Base;
