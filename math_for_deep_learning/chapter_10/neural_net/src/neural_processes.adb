
--  with Ada.Assertions; use Ada.Assertions;
with Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Maths;

with Basic_Printing; use Basic_Printing;
--  with Classifier_Utilities;
with ML_Types;
with Neural_Maths;
with Neural_Utilities;

package body Neural_Processes is

   --     function Backward
   --       (Layer : Layer_Data; Out_Error : Real_Float_Matrix)
   --        return Real_Float_Matrix is
   --        use Neural_Maths;
   --        Result : Real_Float_Matrix (Layer.Input_Data'Range,
   --                                    Layer.Input_Data'Range (2));
   --     begin
   --        for row in Layer.Input_Data'Range loop
   --           for col in Layer.Input_Data'Range (2) loop
   --              Result (row, col) :=
   --                Sigmoid_Deriv (Layer.Input_Data (row, col)) * Out_Error (row, col);
   --           end loop;
   --        end loop;
   --
   --        return Result;

   --     end Backward;

   --  --------------------------------------------------------------

   function Backward
     (Layer : in out Layer_Data; Out_Error : Real_Float_Vector)
      return Real_Float_Vector is
      use Real_Float_Arrays;
      Routine_Name : constant String := "Neural_Processes.Backward ";
      Input_Length  : constant Positive := Integer (Layer.Input_Data.Length);
      In_Error      : Real_Float_Vector (Out_Error'Range);
      In_Data       : Real_Float_Matrix (1 .. 1, 1 .. Input_Length);
   begin
      for index in 1 .. Input_Length loop
         In_Data (1, index) := Layer.Input_Data (index);
      end loop;

      Put_Line (Routine_Name & "Layer Kind: " &
                  Layer_Type'Image (Layer.Layer_Kind));
      Put_Line (Routine_Name & "Out_Error Size" &
                  Integer'Image (Out_Error'Length));
      Print_Matrix_Dimensions (Routine_Name & "In_Data", In_Data);
      if Layer.Layer_Kind = Hidden_Layer then
         declare
            Weights_Error : constant Real_Float_Vector :=
                              Transpose (In_Data) * Out_Error;
            Row_Data      : Real_Float_List;
         begin
            Print_Float_Vector (Routine_Name & "Weights_Error", Weights_Error);
            In_Error := Out_Error *
                          Transpose (To_Real_Float_Matrix (Layer.Weights));
            for row in 1 .. Integer (Layer.Delta_W.Length) loop
               Row_Data := Layer.Delta_W (row);
               for col in 1 .. Integer (Layer.Delta_W.Length) loop
                  Row_Data (col) := Row_Data (col) + Weights_Error (col);
               end loop;
               Layer.Delta_W (row) := Row_Data;
               Layer.Bias (row) := Layer.Bias (row) + Out_Error (row);
            end loop;

            Layer.Passes := Layer.Passes + 1;
         end; --  declare block
      else
         In_Error := Out_Error;
      end if;

      return In_Error;

   end Backward;

   --  --------------------------------------------------------------

   function Forward
     (Layer : in out Layer_Data; Input_Data : Real_Float_List)
      return Real_Float_List is
      use Real_Float_Arrays;
      Routine_Name : constant String := "Neural_Processes.Forward ";
      Out_Data     : Real_Float_List;
   begin
      Layer.Input_Data := Input_Data;
      Put_Line (Routine_Name & "Layer Kind: " &
                  Layer_Type'Image (Layer.Layer_Kind));
      Put_Line (Routine_Name & "Layer Input_Data Size" &
                  Integer'Image (Integer (Layer.Input_Data.Length)));
      if Layer.Layer_Kind = Hidden_Layer then
         Put_Line (Routine_Name & "Layer.Weights Size" &
                     Integer'Image (Integer (Layer.Weights.Length)) & " x" &
                     Integer'Image (Integer (Layer.Weights (1).Length)));
         declare
            New_Data : constant Real_Float_Vector :=
                         To_Real_Float_Vector (Layer.Input_Data) *
                         To_Real_Float_Matrix (Layer.Weights) +
                         To_Real_Float_Vector (Layer.Bias);
         begin
            Put_Line (Routine_Name & "New_Data Length" &
                        Integer'Image (Integer (New_Data'Length)));
            for index in New_Data'Range loop
               Out_Data.Append (New_Data (index));
            end loop;
         end;

      else  --  Activation_Layer
         Out_Data := Input_Data;
      end if;

      return Out_Data;

   end Forward;

   --  --------------------------------------------------------------

   procedure Initialize
     (Layer : out Layer_Data; Input_Size, Output_Size : Positive) is
      Row_List : Real_Float_List;
   begin
      Layer.Weights.Clear;
      Layer.Bias.Clear;
      for row in 1 .. Input_Size loop
         for col in 1 .. Output_Size loop
            Row_List.Append (0.5 * Maths.Random_Float);
         end loop;
         Layer.Weights.Append (Row_List);
         Layer.Bias.Append (0.5 * Maths.Random_Float);
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

   function Loss (Y_True : Real_Float_Vector; Y_Pred : Real_Float_List)
                  return Float is
      use Real_Float_Arrays;
   begin
      return 0.5 * Neural_Maths.Mean
        ((Y_True - To_Real_Float_Vector (Y_Pred)) ** 2);

   end Loss;

   --  ------------------------------------------------------------------------

   function Loss_Deriv (Y_True : Real_Float_Vector; Y_Pred : Real_Float_List)
                        return Real_Float_Vector is
      use Real_Float_Arrays;
      --        Routine_Name : constant String := "Neural_Processes.Loss_Deriv ";
   begin
      return Y_True - To_Real_Float_Vector (Y_Pred);

   end Loss_Deriv;

   --  --------------------------------------------------------------

   procedure Step (Layer : Layer_Data) is
   begin
      null;

   end Step;

   --  --------------------------------------------------------------

   procedure Step (Layer : in out Layer_Data; Eta : Float) is
      --        use Real_Float_Arrays;
      Eta_Av      : constant Float := Eta / Float (Layer.Passes);
      Row_List    : Real_Float_List;
      Delta_W_Row : Real_Float_List;
   begin
      for row in 1 .. Integer (Layer.Weights.Length) loop
         Row_List := Layer.Weights (row);
         Delta_W_Row := Layer.Delta_W (row);
         for col in 1 .. Integer (Row_List.Length) loop
            Row_List (col) := Row_List (col)  - Eta_Av * Delta_W_Row (col);
         end loop;
         Layer.Weights (row) := Row_List;
         Layer.Bias (row) :=
           Layer.Bias (row) - Eta_Av * Layer.Delta_B (row);
         --        Layer.Weights :=
         --          Layer.Weights - Eta_Av * Layer.Delta_W;
         --        Layer.Bias :=
         --           --          Layer.Bias - Eta_Av * Layer.Delta_B;
         --           Layer.Delta_W :=
         --             Zero_Matrix (Layer.Delta_W'Length, Layer.Delta_W'Length (2));
         --           Layer.Delta_B :=
         --             Zero_Array (Layer.Delta_B'Length);
      end loop;
      Layer.Delta_W.Clear;
      Layer.Delta_B.Clear;
      Layer.Passes := 0;

   end Step;

   --  --------------------------------------------------------------

end Neural_Processes;
