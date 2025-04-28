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
  strategy: {
    //'fail-fast': false,
    matrix: {
      os: [
        'ubuntu-latest',
        //TODO: 'macos-latest'
        //TODO: 'windows-latest'
      ],
      'ocaml-compiler': [
        // Old OCaml version in which I ported the ocamlrun to plan9, which is
	// why we care about it here. It does not support |> nor bytes though so
        // we need stdcompat to compile without issues.
        // Note that the |> operator was introduced in 4.02.0, which is a version 
        // we could add in the matrix, but stdcompat does not compile with it.
        '3.10.0',
        // First OCaml version with a working ocamlformat OPAM package
        '4.04.1',
        // Current version I usually work with
        '4.14.1',
        // Why not, living on the edge!
        '5.2.0',
      ],
    },
  },
  'runs-on': '${{ matrix.os }}',
  steps: [
    checkout,
    {
      uses: 'ocaml/setup-ocaml@v2',
      with: {
        'ocaml-compiler': '${{ matrix.ocaml-compiler }}',
        // available only for OCaml >= 4.0.0 and we want also 3.10.0
        'opam-depext': false,
      },
    },
    {
      name: 'Install dependencies',
      //TODO: we should get rid of rc at some point and use the one in xix
      // alt: install 9base (ubuntu package name of plan9port)
      run: |||
        opam install --deps-only .
        sudo apt-get install -y rc
      |||,
    },
    {
      name: 'Build mk/rc',
      run: |||
        eval $(opam env)
        ./bootstrap-mk.sh
      |||,
    },
    {
      name: 'Build all xix',
      run: |||
        eval $(opam env)
        export MKSHELL=`which rc`
        export PATH=`pwd`/bin:$PATH
        mk depend
        mk
      |||,
    },
    {
      name: 'Basic test',
      run: |||
        ./test.sh
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
