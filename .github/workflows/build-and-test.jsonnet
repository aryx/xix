// ----------------------------------------------------------------------------
// Helpers
// ----------------------------------------------------------------------------

local checkout = {
  uses: 'actions/checkout@v3',
};

local build_script = |||
        ./configure
        . ./env
        mk
        mk install
|||;

// ----------------------------------------------------------------------------
// The jobs
// ----------------------------------------------------------------------------

// Arch
local build_x86_linux_arch_job = {
  'runs-on': 'ubuntu-latest',
  container: 'archlinux',
  steps: [
    checkout,
    {
      name: 'Install dependencies',
      run: |||
	pacman -Sy --noconfirm gcc lib32-glibc lib32-gcc-libs
      |||,
    },
    {
      name: 'Build',
      run: build_script,
    }
  ]
};

// Alpine
//local build_x86_linux_alpine_job = {
//  'runs-on': 'ubuntu-latest',
//  // alpine does not support gcc-multilib like ubuntu/arch
//  // so simpler to start from a 32 bits alpine image
//  // But this does not seem to work well with GHA :(
//  container: 'i386/alpine',
//  steps: [
//    checkout,
//    {
//      name: 'Install dependencies',
//      run: |||
//	apk add bash gcc
//      |||,
//    },
//    {
//      name: 'Build',
//      run: build_script,
//    }
//  ]
//};

// Ubuntu
local build_x86_linux_ubuntu_job = {
  'runs-on': 'ubuntu-latest',
  steps: [
    checkout,
    {
      name: 'Install dependencies',
      run: |||
	sudo apt update
	sudo apt-get install -y gcc-multilib
      |||,
    },
    {
      name: 'Build',
      run: build_script,
    }
  ]
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
  },
  jobs: {
    'build-x86-linux-arch': build_x86_linux_arch_job,
    //'build-x86-linux-alpine': build_x86_linux_alpine_job,
    'build-x86-linux-ubuntu': build_x86_linux_ubuntu_job,
  },
}
