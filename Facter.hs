module Facter where

import Data.Char
import Data.List
import Text.Printf
import qualified Data.Set as Set

storageunits = [ ("", 0), ("K", 1), ("M", 2), ("G", 3), ("T", 4) ]

getPrefix :: Int -> String
getPrefix n | null fltr = error $ "Could not get unit prefix for order " ++ (show n)
            | otherwise = fst $ head fltr
    where fltr = filter (\(_, x) -> x == n) storageunits

getOrder :: String -> Int
getOrder n | null fltr = error $ "Could not get order for unit prefix " ++ (show n)
           | otherwise = snd $ head fltr
    where
        nu = map toUpper n
        fltr = filter (\(x, _) -> x == nu) storageunits

normalizeUnit :: (Double, Int) -> Double -> (Double, Int)
normalizeUnit (unit, order) base | unit > base = normalizeUnit (unit/base, order + 1) base
                                 | otherwise = (unit, order)

storagedesc :: (String, String) -> String
storagedesc (ssize, unit) = let
    size = (read ssize) :: Double
    uprefix | unit == "B" = ""
            | otherwise = [head unit]
    uorder = getOrder uprefix
    (osize, oorder) = normalizeUnit (size, uorder) 1024
    in printf "%.2f %sB" osize (getPrefix oorder)

factRAM = do
    meminfo <- readFile "/proc/meminfo" >>= return . map words . lines
    let memtotal  = ginfo "MemTotal:"
        memfree   = ginfo "MemFree:"
        swapfree  = ginfo "SwapFree:"
        swaptotal = ginfo "SwapTotal:"
        ginfo st  = sdesc $ head $ filter (\(x:xs) -> x == st) meminfo
        sdesc [_, size, unit] = storagedesc (size, unit)
    return [("memorysize", memtotal), ("memoryfree", memfree), ("swapfree", swapfree), ("swapsize", swaptotal)]

factNET = do
    return [("ipaddress", "192.168.0.1")]

factOS :: IO [(String, String)]
factOS = do
    lsb <- readFile "/etc/lsb-release" >>= return . map (break (== '=')) . lines
    hostname <- readFile "/proc/sys/kernel/hostname" >>= return . head . lines
    let getval st | null filtered = "?"
                  | otherwise = rvalue
                  where filtered = filter (\(k,_) -> k == st) lsb
                        value    = (tail . snd . head) filtered
                        rvalue | head value == '"' = read value
                               | otherwise         = value
        release = getval "DISTRIB_RELEASE"
        distid  = getval "DISTRIB_ID"
        maj     | release == "?" = "?"
                | otherwise = fst $ break (== '.') release
        osfam   | distid == "Ubuntu" = "Debian"
                | otherwise = distid
    return  [ ("lsbdistid"              , distid)
            , ("operatingsystem"        , distid)
            , ("lsbdistrelease"         , release)
            , ("operatingsystemrelease" , release)
            , ("lsbmajdistrelease"      , maj)
            , ("osfamily"               , osfam)
            , ("hostname"               , hostname)
            , ("lsbdistcodename"        , getval "DISTRIB_CODENAME")
            , ("lsbdistdescription"     , getval "DISTRIB_DESCRIPTION")
            ]

factMountPoints :: IO [(String, String)]
factMountPoints = do
    mountinfo <- readFile "/proc/mounts" >>= return . map words . lines
    let ignorefs = Set.fromList 
                    ["NFS", "nfs", "nfs4", "nfsd", "afs", "binfmt_misc", "proc", "smbfs",
                    "autofs", "iso9660", "ncpfs", "coda", "devpts", "ftpfs", "devfs",
                    "mfs", "shfs", "sysfs", "cifs", "lustre_lite", "tmpfs", "usbfs", "udf",
                    "fusectl", "fuse.snapshotfs", "rpc_pipefs", "configfs", "devtmpfs",
                    "debugfs", "securityfs", "ecryptfs", "fuse.gvfs-fuse-daemon", "rootfs"
                    ]
        goodlines = filter (\x -> not $ Set.member (x !! 2) ignorefs) mountinfo
        goodfs = map (\x -> x !! 1) goodlines
    return [("mountpoints", intercalate " " goodfs)]



version = return [("facterversion", "0.1"),("environment","test")]

allFacts :: IO [(String, String)]
allFacts = mapM id [factNET, factRAM, factOS, version, factMountPoints] >>= return . concat
