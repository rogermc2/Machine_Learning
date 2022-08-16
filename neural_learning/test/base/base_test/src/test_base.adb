
with Ada.Text_IO; use Ada.Text_IO;

with Base_Neural; use Base_Neural;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

procedure Test_Base is

    Routine_Name  : constant String := "Test_Base ";
    Y_True        : constant Real_Float_Matrix (1 .. 3, 1 .. 1) :=
                      ((1 => 0.0), (1 => 0.0), (1 => 1.0));
    Y_Prob        : constant Real_Float_Matrix (1 .. 3, 1 .. 1) :=
                      ((1 => 0.9), (1 => 1.0), (1 => 1.0));

begin
    Put_Line (Routine_Name);
    --  test_log_loss_1_prob_finite
    Put_Line ("Log loss:" & Float'Image (Log_Loss (Y_True, Y_Prob)));
    Put_Line ("Binary log loss:" &
                Float'Image (Binary_Log_Loss (Y_True, Y_Prob)));

end Test_Base;
