
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Numerics.Float_Random;

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

package Connectionist_Types is

    Max_Data     : constant Positive := 10;
    Max_Patterns : constant Positive := 10;

    type Neural_Node_Class
      (Num_Patterns, Num_Inputs, Num_Weights : Positive) is record
        Entries   : Real_Float_Matrix (1 .. Num_Patterns, 1 .. Num_Inputs);
        Weights   : Real_Float_Vector (1 .. Num_Weights);
        Activ     : Float;   --  activation signal
        Output    : Float;   --  exit
        Tendencon : Boolean; --  indicates if the trend is active
        Trend     : Float;   --  trend value}
        Trained   : Boolean := False; --  the neuron is trained
    end record;

    package Data_Values_Package is new
      Ada.Containers.Vectors (Positive, Float);
    subtype Data_Value_List is Data_Values_Package.Vector;

    package Neural_Net_Package is new
      Ada.Containers.Indefinite_Vectors (Positive, Neural_Node_Class);
    subtype Neural_Net_List is Neural_Net_Package.Vector;

    subtype Pattern_Type is Real_Float_Matrix;
    subtype Output_Type is Real_Float_Vector;

end Connectionist_Types;
