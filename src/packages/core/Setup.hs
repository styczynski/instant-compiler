import Buildtools
import Distribution.Types.GenericPackageDescription (emptyGenericPackageDescription)

customHook originalFn = originalFn

main = defaultMainWithHooks $ let UserHooks { readDesc = readDescDefault } = simpleUserHooks in simpleUserHooks { readDesc = customHook readDescDefault }
