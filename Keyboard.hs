module Keyboard (Keyboard
                ,pressed
                ,keyboardCallback
                ,initialKeyboardState) where
import Data.IORef
import qualified Data.Set as Set
import qualified Graphics.UI.GLUT as GLUT


newtype Keyboard = Keyboard (Set.Set GLUT.Key)

pressed :: GLUT.Key -> Keyboard -> Bool
pressed key (Keyboard set) = Set.member key set

keyboardCallback :: IORef Keyboard -> GLUT.KeyboardMouseCallback
keyboardCallback ref key GLUT.Down _ _ = modifyIORef ref add 
  where add (Keyboard set) = Keyboard $ Set.insert key set
keyboardCallback ref key GLUT.Up _ _ = modifyIORef ref remove
  where remove (Keyboard set) = Keyboard $ Set.delete key set

initialKeyboardState :: Keyboard
initialKeyboardState = Keyboard Set.empty
