--  A collection of Model instances for use with fitting packages
--  based on scipy-1.4.1.odr.models.py

package body Models is

   function Linear_Function (B, X : Classifier_Types.Float_List)
                             return Classifier_Types.Float_List is
      B_Sum : Float := 0.0;
      Y     : Classifier_Types.Float_List;
   begin
      for index in B.First_Index + 1 .. B.Last_Index loop
         B_Sum := B_Sum + B (index);
      end loop;

      for index in X.First_Index .. X.Last_Index loop
         Y.Append (B.First_Element + B_Sum * X (index));
      end loop;
      return Y;
   end Linear_Function;

   --  -------------------------------------------------------------------------

end Models;
