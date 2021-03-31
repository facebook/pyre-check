#!/usr/bin/env dotslash

// @generated SignedSource<<87e9d0e078c3cfbb030e92061ae1a89f>>
// MSDK schedule URL: https://our.intern.facebook.com/intern/msdk/bump/?action=view&schedule_fbid=434102841026524
// sandcastle build trigger URL: https://our.intern.facebook.com/intern/sandcastle/job/18014398912084642/

{
    "name": "pyre_bento_integration",
    "oncall": "oncall+pyre@xmail.facebook.com",
    "build_info": {
        "config_script": "fbcode/tools/pyre/facebook/scripts/dotslash/bento.dotslash.py",
        "commit_repo": "fbsource",
        "commit_hash": "7bc2055d70b86b21deba16d9563391bb346852e0",
        "commit_date": "2021-03-31 11:08:46 PDT",
        "commit_timestamp": 1617214126,
        "msdk_package_fbid": "351923636198036"
    },
    "signatures": {
        "msdk": "<<crypto.managed_sdk.mac:AeXs-sqeIEOnvAD9m5YUKf30FyeM7nOkVr4USWU90gW4k3Av>>"
    },
    "platforms": {
        "linux": {
            "scheme": "everstore",
            "handle": "GICWmABL7URhW64BAFLCE1yDSZFGblMqAAAA",
            "extract": {
                "path": "bento_pyre_server",
                "decompress": "tar.gz"
            },
            "metadata": {
                "build_inputs": {
                    "bento_pyre_server": {
                        "cwd": "fbcode",
                        "flags": [
                            "@fbsource//fbcode/mode/opt"
                        ],
                        "target": "//tools/pyre/facebook/scripts/bento:bento_pyre_server"
                    }
                }
            }
        }
    }
}
