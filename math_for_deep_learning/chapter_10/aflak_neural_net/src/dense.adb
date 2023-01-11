
--  with Ada.Text_IO; use Ada.Text_IO;

with Maths;

--  with Basic_Printing; use Basic_Printing;

package body Dense is

   procedure Backward
     (Layer         : in out Layer_Data; Out_Gradient : in out Real_Float_List;
      Learning_Rate : Float) is
      use Maths.Float_Math_Functions;
      use Real_Float_Arrays;
--        Routine_Name : constant String := "Dense.Backward ";
      Input_Data_T : constant Real_Float_Matrix :=
                       Transpose (Real_Float_Matrix (Layer.Input_Data));
      Out_Grad     : constant Real_Float_Matrix :=
                       To_Real_Float_Matrix (Out_Gradient);
   begin
      if Layer.Layer_Kind = Dense_Layer then
         declare
            Weights_T        :  constant Real_Float_Matrix :=
                                 Transpose (Real_Float_Matrix (Layer.Weights));
            Weights_Gradient :  constant Real_Float_Matrix :=
                                 Out_Grad * Input_Data_T;
            Input_Gradient   : constant Real_Float_Matrix :=
                                 Weights_T * Out_Grad;
         begin
            Layer.Weights :=
              Layer_Matrix (Real_Float_Matrix (Layer.Weights) -
                                Learning_Rate * Weights_Gradient);
            Layer.Bias :=
              Layer_Matrix (Real_Float_Matrix (Layer.Bias) -
                                Learning_Rate * Out_Grad);

            Out_Gradient.Clear;
            for row in Input_Gradient'Range loop
               Out_Gradient.Append (Input_Gradient (row, 1));
            end loop;
         end;

      else  --  Activation layer
         for index in Out_Gradient.First_Index .. Out_Gradient.Last_Index loop
            Out_Gradient.Replace_Element
              (index, Out_Gradient (index) *
               (1.0 - Tanh (Out_Gradient (index) ** 2)));
         end loop;
      end if;

   end Backward;

   --  --------------------------------------------------------------

   procedure Forward
     (Layer : in out Layer_Data; Data : in out Real_Float_List) is
      use Maths.Float_Math_Functions;
      use Real_Float_Arrays;
      --        Routine_Name : constant String := "Dense.Forward ";
      In_Data      : constant Real_Float_Matrix := To_Real_Float_Matrix (Data);
   begin
      Layer.Input_Data := Layer_Matrix (In_Data);
      if Layer.Layer_Kind = Dense_Layer then
         declare
            Out_Mat : constant Real_Float_Matrix :=
                        Real_Float_Matrix (Layer.Weights) * In_Data +
                        Real_Float_Matrix (Layer.Bias);
         begin
            Data.Clear;
            for row in Out_Mat'Range loop
               Data.Append (Out_Mat (row, 1));
            end loop;
         end;

      else  --  Tanh Layer
         for index in Data.First_Index .. Data.Last_Index loop
            Data.Replace_Element (index, Tanh (Data (index)));
         end loop;
      end if;

   end Forward;

   --  --------------------------------------------------------------

end Dense;
