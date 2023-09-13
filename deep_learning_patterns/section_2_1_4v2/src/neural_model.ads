with Ada.Containers.Indefinite_Vectors;
with Ada.Numerics.Generic_Real_Arrays;

with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with Stochastic_Optimizers;

package Neural_Model is

   --  Sigmoid_Activation for binary output
   --  Soft_Max_Activation other output types
   type Activation_Kind is (Identity_Activation, Logistic_Activation,
                            ReLu_Activation, Sigmoid_Activation,
                            Soft_Max_Activation);
   type Loss_Kind is (Loss_Binary_Log, Loss_Log, Loss_Mean_Square_Error);

   subtype Node is Float;

   package Activation_Package is new
     Ada.Containers.Indefinite_Vectors (Positive, Activation_Kind);
   subtype Activation_List is Activation_Package.Vector;

   type Layer (Num_Features : Positive) is record
      Nodes : Real_Float_Vector (1 .. Num_Features);
   end record;

   package Layer_Packge is new
     Ada.Containers.Indefinite_Vectors (Positive, Layer);
   subtype Layer_List is Layer_Packge.Vector;

   type Sequential_Model (Num_Samples  : Positive;
                          Num_Features : Positive;
                          Loss_Method  : Loss_Kind) is private;

   procedure Add_Layer (aModel     : in out Sequential_Model;
                        Num_Nodes  : Positive;
                        Input_Data : Real_Float_Vector);
   procedure Add_Layer (aModel     : in out Sequential_Model;
                        Num_Nodes  : Positive);
   procedure Compile (aModel      : in out Sequential_Model);
   --     function Get_Output_Value (aModel : Sequential_Model)
   --                                return Real_Float_Vector;

private
   type Sequential_Model (Num_Samples  : Positive;
                          Num_Features : Positive;
                          Loss_Method  : Loss_Kind) is record
      Input_Data    : Real_Float_Vector (1 .. Num_Features);
      Labels        : Real_Float_Vector (1 .. 1);
      Layers        : Layer_List;
      Connections   : Stochastic_Optimizers.Parameters_List;
      Activations   : Activation_List;
      Delta_Weights : Real_Float_Vector (1 .. Num_Features) :=
                        (others => 0.0);
      Delta_Bias    : Real_Float_Vector (1 .. Num_Features) :=
                        (others => 0.0);
   end record;

end Neural_Model;
