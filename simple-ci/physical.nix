let

  region = "us-west-1";
  accessKeyId = "default";

  ec2 =
    { resources, ... }:
    { deployment.targetEnv = "ec2";
      deployment.ec2.accessKeyId = accessKeyId;
      deployment.ec2.region = region;
      deployment.ec2.instanceType = "t2.nano";
      deployment.ec2.keyPair = resources.ec2KeyPairs.my-key-pair;
    };

in
{ machine = ec2;

  # Provision an EC2 key pair.
  resources.ec2KeyPairs.my-key-pair =
    { inherit region accessKeyId; };
}
