--  Based on scikit-learn/sklearn/neural_network/_base.py

with Maths;

package body Base is

   function Identity (X : Float_List_2D) return Float_List_2D is
   begin
      return X;
   end Identity;

   --  -------------------------------------------------------------------------

   function Logistic (X : Float_List_2D) return Float_List_2D is
   begin
      return X;
   end Logistic;

   --  -------------------------------------------------------------------------

   function Tanh (X : Float_List_2D) return Float_List_2D is
      use Maths.Float_Math_Functions;
      X_Col  : Float_List;
      Result : Float_List_2D;
   begin
      for row in X.First_Index .. X.Last_Index loop
         X_Col := X.Element (row);
         for index in X_Col.First_Index .. X_Col.Last_Index loop
               X_Col.Replace_Element (index, Tanh (X_Col.Element (index)));
         end loop;
         Result.Append (X_Col);
      end loop;

      return Result;
   end Tanh;

   --  -------------------------------------------------------------------------

   function Relu (X : Float_List_2D) return Float_List_2D is
      X_Col  : Float_List;
      Result : Float_List_2D;
   begin
      for row in X.First_Index .. X.Last_Index loop
         X_Col := X.Element (row);
         for index in X_Col.First_Index .. X_Col.Last_Index loop
            X_Col.Replace_Element
              (index, Float'Max (0.0, X_Col.Element (index)));
         end loop;
         Result.Append (X_Col);
      end loop;

      return Result;
   end Relu;

   --  -------------------------------------------------------------------------

   function Softmax (X : Float_List_2D) return Float_List_2D is
      X_Col  : Float_List;

      Result : Float_List_2D;
   begin
      for row in X.First_Index .. X.Last_Index loop
         X_Col := X.Element (row);
         for index in X_Col.First_Index .. X_Col.Last_Index loop
            X_Col.Replace_Element
              (index, Float'Max (0.0, X_Col.Element (index)));
         end loop;
         Result.Append (X_Col);
      end loop;

      return Result;
   end Softmax;

   --  -------------------------------------------------------------------------

end Base;
