
with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

--  with Basic_Printing; use Basic_Printing;
with ML_Types;
with Neural_Maths;
with Neural_Utilities;

package body Neural_Processes is

   function Backward
     (Layer : in out Layer_Data; Out_Error : Real_Float_List)
      return Real_Float_List is
      use Real_Float_Arrays;
      --        Routine_Name  : constant String := "Neural_Processes.Backward ";
      Out_Error_Vec : constant Real_Float_Vector :=
                        To_Real_Float_Vector (Out_Error);
      In_Error      : Real_Float_Vector (1 .. Integer (Layer.Input_Size));
      Weights_Error : Real_Float_Vector (Out_Error_Vec'Range);
   begin
      --        Put_Line (Routine_Name & "Layer Kind: " &
      --                    Layer_Type'Image (Layer.Layer_Kind));
      --        Put_Line (Routine_Name & "Out_Error Size" &
      --                    Integer'Image (Out_Error_Vec'Length));
      --        Put_Line (Routine_Name & "Layer.Input_Data" &
      --                    Integer'Image (Layer.Input_Data'Length));
      if Layer.Layer_Kind = Hidden_Layer then

         In_Error := Out_Error_Vec *
           Transpose (Real_Float_Matrix (Layer.Weights));

         for index in Out_Error_Vec'Range loop
            Weights_Error (index) :=
              Layer.Input_Data (Layer_Range (index)) * Out_Error_Vec (index);
         end loop;
         --           Print_Float_Vector (Routine_Name & "Weights_Error", Weights_Error);

         --  accumulate the error over a minibatch
         for row in Layer.Delta_W'Range loop
            for col in Layer.Delta_W'Range (2) loop
               Layer.Delta_W (row,col) := Layer.Delta_W (row,col) +
                 Weights_Error (Integer (col));
            end loop;
         end loop;

         for index in Layer.Bias'Range loop
            Layer.Bias (index) :=
              Layer.Bias (index) + Out_Error (Integer (index));
         end loop;

         Layer.Passes := Layer.Passes + 1;
      else  --  Actvation layer
         for index in Out_Error_Vec'Range loop
            In_Error (index) :=
              Neural_Maths.Sigmoid_Deriv (Real_Float_Vector (Layer.Input_Data) (index)) *
                Out_Error (index);
         end loop;
      end if;

      return To_Real_Float_List (In_Error);

   end Backward;

   --  --------------------------------------------------------------

   function Forward
     (Layer : in out Layer_Data; Input_Data : Real_Float_List)
      return Real_Float_List is
      use Real_Float_Arrays;
      Routine_Name : constant String := "Neural_Processes.Forward ";
      In_Data      : constant Real_Float_Vector :=
                       To_Real_Float_Vector (Input_Data);
      Out_Data     : Real_Float_List;
   begin
      --        Put_Line (Routine_Name & "Layer Kind: " &
      --                    Layer_Type'Image (Layer.Layer_Kind));
      Assert (Layer.Input_Data'Length = Integer (Input_Data.Length),
              Routine_Name & "Input_Data length" &
                Integer'Image (Integer (Input_Data.Length)) &
                " differs from layer Input_Data length" &
                Integer'Image (Layer.Input_Data'Length));
      if Layer.Layer_Kind = Hidden_Layer then
         Layer.Input_Data := Layer_Vector (In_Data);
         Out_Data :=
           To_Real_Float_List (Real_Float_Vector (Layer.Input_Data) *
                                   Real_Float_Matrix (Layer.Weights) +
                                   Real_Float_Vector (Layer.Bias));

      else  --  Activation_Layer
         Layer.Input_Data := Layer_Vector (In_Data);

         Out_Data := To_Real_Float_List (Neural_Maths.Sigmoid (In_Data));
      end if;

      return Out_Data;

   end Forward;

   --  --------------------------------------------------------------

   procedure Initialize
     (Layer : out Layer_Data; Input_Size, Output_Size : Layer_Range) is
   begin
      for row in 1 .. Input_Size loop
         for col in 1 .. Output_Size loop
            Layer.Weights (row, col) := 0.5 * Maths.Random_Float;
         end loop;
         Layer.Bias (row) := 0.5 * Maths.Random_Float;
      end loop;

   end Initialize;

   --  --------------------------------------------------------------

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

   function MSE_Derivative (Y_True, Y_Pred : Real_Float_Vector)
                            return Real_Float_Vector is
      use Real_Float_Arrays;
      --        Routine_Name : constant String := "Neural_Processes.Loss_Deriv ";
   begin
      --  original code had Y_Pred - Y_True which is inconsistent with
      --  Mean_Square_Error declaration
      return Y_True - Y_Pred;

   end MSE_Derivative;

   --  --------------------------------------------------------------

   procedure Step (Layer : in out Layer_Data; Eta : Float) is
      use Real_Float_Arrays;
      --        Routine_Name : constant String := "Neural_Processes.Step Hidden_Layer ";
   begin
      if Layer.Layer_Kind = Hidden_Layer then
         declare
            Eta_Av : constant Float := Eta / Float (Layer.Passes);
            ZM     : constant Real_Float_Matrix :=
                       Zero_Matrix (Layer.Delta_W'Length, Layer.Delta_W'Length (2));
         begin
            Layer.Weights :=
              Layer_Matrix (Real_Float_Matrix (Layer.Weights) -
                                Eta_Av * Real_Float_Matrix (Layer.Delta_W));

            Layer.Bias :=
              Layer_Vector (Real_Float_Vector (Layer.Bias) -
                                Eta_Av * Real_Float_Vector (Layer.Delta_B));

            Layer.Delta_W := Layer_Matrix (ZM);
            Layer.Delta_B := Layer_Vector (Zero_Array (Layer.Delta_B'Length));

            Layer.Passes := 0;
         end;  -- declare block
      end if;  --  Hidden layer

   end Step;

   --  --------------------------------------------------------------

end Neural_Processes;
