
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

--  with Basic_Printing; use Basic_Printing;
with ML_Types;
with Neural_Maths;
with Neural_Utilities;

package body Neural_Processes is

   procedure Backward
     (Layer         : in out Layer_Data; Out_Gradient : in out Real_Float_List;
      Learning_Rate : Float) is
      use Real_Float_Arrays;
      --        Routine_Name  : constant String := "Neural_Processes.Backward ";
      Data_T        : constant Real_Float_Matrix :=
        Transpose (Real_Float_Matrix (Layer.Input_Data));
      Out_Grad      : constant Real_Float_Matrix :=
        To_Real_Float_Matrix (Out_Gradient);
   begin
      --        Put_Line (Routine_Name & "Layer Kind: " &
      --                    Layer_Type'Image (Layer.Layer_Kind));
      --        Print_Matrix_Dimensions (Routine_Name & "Input_Data",
      --                                 Real_Float_Matrix (Layer.Input_Data));
      --        Print_Matrix_Dimensions (Routine_Name & "Out_Grad", Out_Grad);

      if Layer.Layer_Kind = Hidden_Layer then
         declare
            Weights_T        :  constant Real_Float_Matrix :=
              Transpose (Real_Float_Matrix (Layer.Weights));
            Weights_Gradient :  constant Real_Float_Matrix := Out_Grad * Data_T;
            Input_Gradient   : constant Real_Float_Matrix :=
              Weights_T * Out_Grad;
         begin
            --              Print_Matrix_Dimensions (Routine_Name & "Input_Data",
            --                                 Real_Float_Matrix (Layer.Input_Data));
            Layer.Weights :=
              Layer_Matrix (Real_Float_Matrix (Layer.Weights) -
                                Learning_Rate * Weights_Gradient);
            Layer.Bias :=
              Layer_Matrix (Real_Float_Matrix (Layer.Bias) -
                                Learning_Rate * Out_Grad);
            Layer.Delta_W :=
              Layer_Matrix (Real_Float_Matrix (Layer.Delta_W) + Out_Grad * Data_T);
            Layer.Delta_B :=
              Layer_Matrix (Real_Float_Matrix (Layer.Delta_B) + Out_Grad);
            Layer.Passes := Layer.Passes + 1;

            Out_Gradient.Clear;
            for row in Input_Gradient'Range loop
               Out_Gradient.Append (Input_Gradient (row, 1));
            end loop;
         end;

      else  --  Activation layer
         declare
            In_Error : constant Real_Float_Matrix
              := H_Product (Neural_Maths.Sigmoid_Deriv
                            (Real_Float_Matrix (Layer.Input_Data)), Out_Grad);
         begin
            Out_Gradient.Clear;
            for row in In_Error'Range loop
               Out_Gradient.Append (In_Error (row, 1));
            end loop;
         end;
      end if;

   end Backward;

   --  --------------------------------------------------------------

   procedure Forward
     (Layer : in out Layer_Data; Data : in out Real_Float_List) is
      use Real_Float_Arrays;
      Routine_Name : constant String := "Neural_Processes.Forward ";
      In_Data      : constant Real_Float_Matrix := To_Real_Float_Matrix (Data);
   begin
      Layer.Input_Data := Layer_Matrix (In_Data);
      --        Put_Line (Routine_Name & "Layer Kind: " &
      --                    Layer_Type'Image (Layer.Layer_Kind));
      --        Print_Float_Matrix (Routine_Name & "transposed Layer.Input_Data",
      --                              Transpose (Real_Float_Matrix (Layer.Input_Data)), 1, 1, 50, 100);
      if Layer.Layer_Kind = Hidden_Layer then
         declare
            Out_Mat : constant Real_Float_Matrix :=
              Real_Float_Matrix (Layer.Weights) * In_Data +
              Real_Float_Matrix (Layer.Bias);
         begin
            for row in Layer.Input_Data'Range loop
               Assert (Layer.Input_Data (Layer_Range (row), 1)'Valid,
                       Routine_Name & "Hidden_Layer invalid Layer.Input_Data " &
                         Float'Image (Layer.Input_Data (Layer_Range (row), 1)));
            end loop;

            Data.Clear;
            for row in Out_Mat'Range loop
               Data.Append (Out_Mat (row, 1));
            end loop;

            for row in Data.First_Index .. Data.Last_Index loop
               Assert (Data.Element (row)'Valid, Routine_Name &
                         "Hidden_Layer invalid Data " & Float'Image (Data (row)));
            end loop;
            --              Print_Real_Float_List (Routine_Name & "output Data", Data, 1, 40);
         end;

      else  --  Activation_Layer
         Data.Clear;
         for row in Layer.Input_Data'Range loop
            Assert (Layer.Input_Data (Layer_Range (row), 1)'Valid,
                    Routine_Name & "Activation_Layer invalid Layer.Input_Data row" &
                      Layer_Range'Image (row) & ":  " &
                      Float'Image (Layer.Input_Data (Layer_Range (row), 1)));
            Data.Append (Neural_Maths.Sigmoid (Layer.Input_Data (row, 1)));
         end loop;
         --           Print_Real_Float_List (Routine_Name & "Data", Data);

      end if;

   end Forward;

   --  --------------------------------------------------------------

   procedure Gradient_Descent_Step (Layer : in out Layer_Data; Eta : Float) is
      use Real_Float_Arrays;
      --        Routine_Name : constant String := "Neural_Processes.Gradient_Descent_Step Hidden_Layer ";
      Eta_Av : Float;
   begin
      if Layer.Layer_Kind = Hidden_Layer then
         Eta_Av := Eta / Float (Layer.Passes);
         --              Print_Float_Matrix (Routine_Name & "Layer.Delta_W",
         --                                  Real_Float_Matrix (Layer.Delta_W), 1, 2);
         Layer.Weights :=
           Layer_Matrix (Real_Float_Matrix (Layer.Weights) -
                             Eta_Av * Real_Float_Matrix (Layer.Delta_W));

         Layer.Bias :=
           Layer_Matrix (Real_Float_Matrix (Layer.Bias) -
                             Eta_Av * Real_Float_Matrix (Layer.Delta_B));

         --  reset Delta_W and Delta_B for the next minibatch
         declare
            --  declare block prevents ambiguous Zero_Matrix operand problems.
            ZMW : constant Real_Float_Matrix :=
              Zero_Matrix (Layer.Delta_W'Length, Layer.Delta_W'Length (2));
            ZMD : constant Real_Float_Matrix :=
              Zero_Matrix (Layer.Delta_B'Length, Layer.Delta_B'Length (2));
         begin
            Layer.Delta_W := Layer_Matrix (ZMW);
            Layer.Delta_B := Layer_Matrix (ZMD);
         end;  -- declare block

         Layer.Passes := 0;
      end if;  --  Hidden layer

   end Gradient_Descent_Step;

   --  ------------------------------------------------------------------------

   function Load_Data (File_Name : String; Num_Columns : Positive)
                       return Real_Float_Matrix is
      use Ada.Strings.Unbounded;
      CSV_Data  : constant ML_Types.Raw_Data_Vector :=
        Neural_Utilities.Load_Raw_CSV_Data (File_Name);
      List_Row  : ML_Types.Unbounded_List;
      Data      : ML_Arrays_And_Matrices.Real_Float_Matrix
        (1 .. Positive (CSV_Data.Length), 1 .. Num_Columns);
   begin
      Put_Line ("Loading " & File_Name);
      for row in Data'Range loop
         List_Row := CSV_Data (row);
         for col in Data'Range( 2) loop
            Data (row, col) := Float'Value (To_String (List_Row (col)));
         end loop;
      end loop;

      return Data;

   end Load_Data;

   --  -------------------------------------------------------------------------

   function Load_Data (File_Name : String) return Real_Float_Vector is
      use  Ada.Strings.Unbounded;
      CSV_Data : constant ML_Types.Unbounded_List :=
        Neural_Utilities.Load_CSV_Data (File_Name);
      Data     : ML_Arrays_And_Matrices.Real_Float_Vector
        (1 .. Positive (CSV_Data.Length));
   begin
      Put_Line ("Loading " & File_Name);
      for index in CSV_Data.First_Index .. CSV_Data.Last_Index loop
         Data (index) := Float'Value (To_String (CSV_Data (index)));
      end loop;

      return Data;

   end Load_Data;

   --  -------------------------------------------------------------------------

   function Mean_Square_Error (Y_True, Y_Pred : Real_Float_Vector) return Float is
      use Real_Float_Arrays;
   begin
      return 0.5 * Neural_Maths.Mean ((Y_True - Y_Pred) ** 2);

   end Mean_Square_Error;

   --  ------------------------------------------------------------------------

   function Minus_MSE_Derivative (Y_True, Y_Pred : Real_Float_Vector)
                                  return Real_Float_Vector is
      use Real_Float_Arrays;
   begin
      return Y_Pred - Y_True;

   end Minus_MSE_Derivative;

   --  --------------------------------------------------------------

end Neural_Processes;
