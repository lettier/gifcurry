-- original author:
--    Mirco "MacSlow" Mueller <macslow@bangang.de>
--
-- created:
--    10.1.2006 (or so)
--
-- http://www.gnu.org/licenses/licenses.html#GPL
--
-- ported to Haskell (gtk2hs) by:
--    Duncan Coutts <duncan.coutts@worc.ox.ac.uk>
--
-- ported to haskell-gi from the gtk2hs port by:
--    Iñaki García Etxebarria <garetxe@gmail.com>
--
-- Modified by David Lettier

module GiCairoCairoBridge (renderWithContext) where

import Control.Monad.Trans.Reader (runReaderT)
import qualified GI.Cairo
import Graphics.Rendering.Cairo.Internal (Render(runRender))
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))
import Foreign.Ptr (castPtr)

-- | This function bridges gi-cairo with the hand-written cairo
-- package. It takes a `GI.Cairo.Context` (as it appears in gi-cairo),
-- and a `Render` action (as in the cairo lib), and renders the
-- `Render` action into the given context.
renderWithContext :: GI.Cairo.Context -> Render () -> IO ()
renderWithContext ct r = GI.Cairo.withManagedPtr ct $ \p ->
  runReaderT (runRender r) (Cairo (castPtr p))
