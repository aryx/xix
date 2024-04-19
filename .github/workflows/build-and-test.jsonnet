// Build and test using setup-ocaml@v2 mostly.

// ----------------------------------------------------------------------------
// Helpers
// ----------------------------------------------------------------------------

local checkout = {
  uses: 'actions/checkout@v3',
};

// ----------------------------------------------------------------------------
// The job
// ----------------------------------------------------------------------------

local job = {
  // LATER:
  //  strategy:
  //    fail-fast: false
  //    matrix:
  //      os:
  //        - macos-latest
  //        - ubuntu-latest
  //        - windows-latest
  //      ocaml-compiler:
  //        - "5.1"
  //
  //  runs-on: ${{ matrix.os }}
  'runs-on': 'ubuntu-latest',
  steps: [
    checkout,
    {
      uses: "ocaml/setup-ocaml@v2",
      with: {
	// TODO: ${{ matrix.ocaml-compiler }}
	'ocaml-compiler': "4.02.1",
      }
    },
    {
      name: 'Install dependencies',
      run: |||
        echo No dependencies! This is xix! It does not need anything!
      |||,
    },
    {
      name: 'Build mk/rc',
      run: |||
        eval $(opam env)
        ./bootstrap-mk.sh
      |||,
    },
  ],
};

// ----------------------------------------------------------------------------
// The workflow
// ----------------------------------------------------------------------------
{
  name: 'build-and-test',
  on: {
    // can be run manually from the GHA dashboard
    workflow_dispatch: null,
    // on the PR
    pull_request: null,
    // and another time once the PR is merged on master
    push: {
      branches: [
        'master',
      ],
    },
    schedule: [
      {
        // every day at 12:59
        cron: '59 12 * * *',
      },
    ],
  },
  jobs: {
    job: job,
  },
}
