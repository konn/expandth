{-# LANGUAGE DeriveDataTypeable, QuasiQuotes, TemplateHaskell #-}
module Main where
import           Data.ByteString
import qualified Data.ByteString        as BS
import           Data.Typeable
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Language.C.Inline.ObjC
import           Language.C.Quote.ObjC

objc_import ["HsFFI.h", "<Foundation/Foundation.h>"]

newtype NSAppleEventDescriptor =
  NSAppleEventDescriptor (ForeignPtr NSAppleEventDescriptor)
  deriving (Typeable)

newtype NSData =
  NSData (ForeignPtr NSData)
  deriving (Typeable)

__idNSAED :: NSAppleEventDescriptor -> IO NSAppleEventDescriptor
__idNSAED = return

objc_interface [cunit|
const char *fromNSData(typename NSData *dat);
|]

objc_implementation [] [cunit|
const char *fromNSData(typename NSData *dat){
  return (const char *)[dat bytes];
}
|]

objc_marshaller '__idNSAED '__idNSAED

foreign import ccall "test_stub.h fromNSData"
  fromNSData :: Ptr NSData -> IO (Ptr CChar)

dataToByteString :: NSData -> IO ByteString
dataToByteString dat@(NSData fptr) = withForeignPtr fptr $ \ ptr -> do
  len <- $(objc ['dat :> Class ''NSData] $
          ''Int <: [cexp|[dat length]|])
  cstr <- fromNSData ptr
  packCStringLen (cstr, len)

byteStringToData :: ByteString -> IO NSData
byteStringToData bs = useAsCStringLen bs $ \ (cstr, len) -> do
  $(objc ['cstr :> ''CString, 'len :> ''Int] $
    Class ''NSData <: [cexp|[NSData dataWithBytes: cstr length: len]|])

objc_marshaller 'byteStringToData 'dataToByteString

objc_typecheck

eventDescriptorData :: NSAppleEventDescriptor -> IO ByteString
eventDescriptorData desc =
  $(objc ['desc :> Class ''NSAppleEventDescriptor] $
    ''ByteString <: [cexp|[desc data]|] )

doAppleScript :: String -> IO NSAppleEventDescriptor
doAppleScript src =
  $(objc ['src :> ''String] $
   Class ''NSAppleEventDescriptor <: [cexp|[[[NSAppleScript alloc] initWithSource: src] executeAndReturnError: NULL]|])


objc_emit


script = "tell application \"Skim\"\n\
         \  activate\n\
         \  tell document 1 \n\
         \    set interaction mode to presentation mode\n\
         \    go to page ((the index of the current page) + 1)\n\
         \    grab the current page of the presentation notes document\n\
         \  end tell\n\
         \end tell"

main :: IO ()
main = do
  objc_initialise
  BS.writeFile "one.pdf" =<< eventDescriptorData =<< doAppleScript script
