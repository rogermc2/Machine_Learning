
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types;
with Stochastic_Optimizers;

package Test_Support is

   function Almost_Equal (A, B : Real_Float_Matrix; Accuracy : Integer)
                           return Boolean;
   function Almost_Equal (A, B : Real_Float_Vector; Accuracy : Integer)
                           return Boolean;
   function Almost_Equal (A, B     : Stochastic_Optimizers.Parameters_Record;
                          Accuracy : Integer) return Boolean;
   procedure Print_Binary_Matrix (Name  : String; aMatrix : Binary_Matrix);
   procedure Print_Float_Array (Name  : String; anArray : Real_Float_Vector;
                                Start : Integer := 1; Finish : Integer := 0);
   procedure Print_Float_Matrix (Name  : String; aMatrix : Real_Float_Matrix);
   procedure Print_Float_Vector (Name : String; Vec : Real_Float_Vector);
   procedure Print_Integer_Array (Name  : String; anArray : Integer_Array;
                                  Start : Integer := 1; Finish : Integer := 0);
   procedure Print_Integer_List (Name  : String;
                                 aList : NL_Types.Integer_List);
   procedure Print_Parameters
     (Name       : String; Params : Stochastic_Optimizers.Parameters_Record;
      Rows_Start : Positive := 1; Rows_Last : Positive := 10);
   procedure Print_Unbound_List (Name : String; UB : NL_Types.Unbounded_List);
   procedure Print_Unbound_Matrix (Name : String;
                                   UB   : Unbounded_String_Matrix);

end Test_Support;
