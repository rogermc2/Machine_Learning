
with Ada.Containers.Indefinite_Vectors;
with Ada.Containers.Vectors;
with Ada.Numerics.Float_Random;

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

package Connectionist_Types is

--      Max_Data   : constant Positive := 10;
--      Max_Layers : constant Positive := 10;

    type Processing_Element
      (Num_Inputs, Num_Weights : Natural) is record
        Entries   : Real_Float_Vector (1 .. Num_Inputs);
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

    package Processing_Element_Package is new
      Ada.Containers.Indefinite_Vectors (Positive, Processing_Element);
    subtype Processing_Element_List is Processing_Element_Package.Vector;

    --  inputs and weights in the neuron:
    --  tdata = array [1..maxdata] of real;
    --  input Layers:
    --  tpatrentr = array [1 .. maxLayers] of tdata;
    --  "Data" includes inputs and weights

    use Real_Float_Arrays;
    subtype Data_Type is Real_Float_Vector;
    package Layer_Package is new
      Ada.Containers.Indefinite_Vectors (Positive, Data_Type);
    subtype Layer_List is Layer_Package.Vector;

    subtype Output_Type is Real_Float_Vector;

end Connectionist_Types;
