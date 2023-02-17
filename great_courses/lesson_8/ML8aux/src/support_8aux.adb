
with Ada.Assertions; use Ada.Assertions;
--  with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;

package body Support_8Aux is

   --  -------------------------------------------------------------------------
   --  Above_Line computes a slope and intercept based on variables x1 and x2.
   --  It returns True if x3[1] is above the line defined by this slope and
   --  the intercept.
   function Above_Line (X1, X2, X3 : Real_Float_Vector) return Boolean is
      M      : constant Float := (X2 (2) - X1 (2)) / (X2 (1) - X1 (1));
      B      : constant Float := X1 (2) - M * X1 (1);
   begin
      return X3 (2) > M * X3 (1) + B;

   end Above_Line;

   --  -------------------------------------------------------------------------

   function Comfort (Temp, Rel_Humid : Float) return Boolean is
      X1a    : constant Real_Float_Vector (1 .. 2) := (86.5, 67.1);
      X1b    : constant Real_Float_Vector (1 .. 2) := (29.3, 69.0);
      X2a    : constant Real_Float_Vector := X1b;
      X2b    : constant Real_Float_Vector (1 .. 2) := (23.0,76.0);
      X3a    : constant Real_Float_Vector := X2b;
      X3b    : constant Real_Float_Vector (1 .. 2) := (58.3,74.3);
      X4a    : constant Real_Float_Vector := X3b;
      X4b    : constant Real_Float_Vector (1 .. 2) := (86.5,67.1);
      H_T    : constant Real_Float_Vector (1 .. 2) := (Rel_Humid, Temp);
   begin
      return
        Above_Line (X1a, X1b, H_T) and
        Above_Line (X2a, X2b, H_T) and
        not Above_Line (X3a, X3b, H_T) and
        not Above_Line (X4a, X4b, H_T);

   end Comfort;

   --  -------------------------------------------------------------------------

   function Get_Predictions (Predictions : Boolean_Array; Labels : Boolean_Array)
                             return Prediction_Info is
      --        Routine_Name : constant String := "Support_8Aux.Get_Predictions ";
      Result      : Prediction_Info (Predictions'Range);
   begin
      for index in Predictions'Range loop
         if not Predictions (index) then
            if not Labels (index) then
               Result (index) := False_Negative;
            else
               Result (index) := False_Positive;
            end if;
         else
            if not Labels (index) then
               Result (index) := True_Negative;
            else
               Result (index) := True_Positive;
            end if;
         end if;
      end loop;

      return Result;

   end Get_Predictions;

   --  -------------------------------------------------------------------------

   procedure Train_Test_Split
     (X          : Real_Float_Matrix; Y : Boolean_Array;
      Train_Size : Natural; Test_Size  : Natural;
      Train_X    : out Real_Float_Matrix; Train_Y : out Boolean_Array;
      Test_X     : out Real_Float_Matrix; Test_Y : out Boolean_Array) is
      Routine_Name : constant String := "Support_8Aux.Train_Test_Split ";
      Num_Samples  : constant Positive := X'Length;
   begin
      Assert (Natural (Y'Length) = Num_Samples, Routine_Name &
                "Y length" & Integer'Image (Integer (Y'Length)) &
                " is different to X length" & Natural'Image (Num_Samples));

      for row in 1 .. Train_Size loop
         for col in Train_X'Range (2) loop
            Train_X (row, col) := X (row, col);
         end loop;
         Train_Y (row) := Y (row);
      end loop;

      for row in 1 .. Test_Size loop
         for col in Test_X'Range (2) loop
            Test_X (row, col) := X (row + Train_Size, col);
         end loop;
         Test_Y (row) := Y (row + Train_Size);
      end loop;

   end Train_Test_Split;

   --  -------------------------------------------------------------------------

   function Accuracy (Predictions : Boolean_Array; Labels : Boolean_Array)
                      return Float is
      --        Routine_Name : constant String := "Support_8Aux.Accuracy ";
      Correct      : Natural := 0;
   begin
      for index in Predictions'Range loop
         if Predictions (index) = Labels (index) then
            Correct := Correct + 1;
         end if;
      end loop;

      return Float (Correct) / Float (Predictions'Length);

   end Accuracy;

   --  -------------------------------------------------------------------------

end Support_8Aux;
