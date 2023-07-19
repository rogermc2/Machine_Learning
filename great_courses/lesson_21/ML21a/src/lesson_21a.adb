
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use  Basic_Printing;
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Python;

with Python_21A;
with Support_21A; use Support_21A;

procedure Lesson_21A is
   Program_Name         : constant String := "Lesson 21A ";
   Num_Rows             : constant Positive := 5;
   Num_Cols             : constant Positive := 10;
   Num_Cats             : constant Positive := 5;
   Grid_Map             : constant Integer_Matrix (1 .. Num_Rows,
                                                   1 .. Num_Cols) :=
                            ((0,0,0,0,0,2,0,0,1,0),
                             (0,1,0,0,0,2,0,0,0,0),
                             (0,0,0,0,0,2,1,3,0,0),
                             (0,0,0,1,0,2,0,3,0,0),
                             (0,0,0,0,0,2,0,3,0,4));
   Rewards              : constant Integer_Array (Grid_Map'Range) :=
                            (0, -1, -1, -1, 10);
   Num_Actions          : constant Positive := 5;
   --  Acts defines how each action changes the row and column
   Actions              : constant Actions_Matrix (1 .. Num_Actions, 1 ..2) :=
                            ((-1,0), (0,1), (1,0), (0,-1), (0,0));
   --  Mat_Map is a binarised version of Grid_Map in which the value of
   --  Mat_Map is 1 (otherwise 0) if the Grid_Map row and column
   --  equals the index of the third dimension of the cell.
   Mat_Map              : constant Boolean_Tensor :=
                            Support_21A.Compute_Map_Matrix
                              (Grid_Map, Num_Cats);
   --  Mat_Transition indicates whether or not a given action will cause
   --  a transition between a given pair of locations.
   Mat_Transition       : constant Boolean_Tensor :=
                            Support_21A.Compute_Transition_Matrix
                              (Num_Rows, Num_Cols, Num_Actions, Actions,
                               Mat_Map);
   Classifier           : Python.Module;
begin
   Python.Initialize;
   Classifier := Python.Import_File ("lesson_21a");
   --     Python.Call (Classifier, "plot", Grid_Map);
   declare
      Result : constant Python_21A.Plan_Data :=
                 Python_21A.Set_Policy (Classifier, Rewards, Mat_Map, Mat_Transition);
   begin
      Plot_Policy (Num_Rows, Num_Cols, Result.Policy, Actions);
   end;

   --     Python.Call (Classifier, "plot_matrix", Grid_Map);
   Python.Finalize;

   Put_Line ("----------------------------------------------");

   Put_Line (Program_Name & "finished.");
   New_Line;

exception
   when Error: Constraint_Error => Put_Line (Program_Name &
                                               "Constraint_Error");
      Put_Line (Exception_Information(Error));
   when Error: others => Put_Line (Program_Name & "exception");
      Put_Line (Exception_Information(Error));

end Lesson_21A;
